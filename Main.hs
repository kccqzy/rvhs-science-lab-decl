{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Control.Exception (bracket)
import Control.Arrow (first, second)
import Data.Either
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Attoparsec.ByteString.Char8 as PC
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Data.Aeson as JSON
import qualified Data.SafeCopy as SafeCopy
import qualified Data.Acid as Acid
import qualified Data.Acid.Local as Acid
import qualified Data.Acid.Advanced as Acid
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as HTTP
import Text.Cassius (cassiusFile)
import Text.Hamlet (hamletFile)
import Text.Julius (juliusFile)
import Text.Jasmine (minifym)
import qualified Codec.Picture.Png as Png
import Yesod.Core
import Yesod.Form
import Yesod.EmbeddedStatic

-- |
-- = Basic Data Structures & Operations
-- The basic data structures used in the project. All data structures
-- are prefix by LD. A small set of permitted operations are also
-- provided that uses the Update and Query monad.

-- | The class, i.e. "5N".
type LDClassName = T.Text

-- | The science subject, combined science (Y1-2), physics, chemistry
-- or bio. Implementatio note: we could have made a newtype instead:
-- > newtype LDScienceSubject = LDScienceSubject { describeSubject :: T.Text }
-- but this means set operations would require traversing the text
-- instead of just the symbol.
data LDScienceSubject = LDScience
                      | LDDnT
                      | LDPhysics
                      | LDChemistry
                      | LDBiomedicalScience
                      | LDH2Physics
                      | LDH2Chemistry
                      | LDH2Biology
                      deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | Give a human readable name for a science subject.
describeSubject :: LDScienceSubject -> T.Text
describeSubject LDScience = "Combined Science"
describeSubject LDDnT = "Design & Technology"
describeSubject LDPhysics = "Physics"
describeSubject LDChemistry = "Chemistry"
describeSubject LDBiomedicalScience = "Biomedical Science"
describeSubject LDH2Physics = "H2 Physics"
describeSubject LDH2Chemistry = "H2 Chemistry"
describeSubject LDH2Biology = "H2 Biology"

instance ToJSON LDScienceSubject where
  toJSON = toJSON . describeSubject
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''LDScienceSubject)

-- | All the information submitted through the UI: phone, email and
-- the submitted signature.
data LDSubmissionStatus = LDNotSubmittedYet
                        | LDSubmitted
                          { getSubmittedPhone :: T.Text,
                            getSubmittedEmail :: T.Text,
                            getSubmittedSignature :: T.Text -- TODO store raw data, not base64
                          } deriving (Show, Eq, Data, Typeable, Generic)
instance ToJSON LDSubmissionStatus
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''LDSubmissionStatus)

-- | The student record.
data LDStudent = LDStudent
                 { getName :: !T.Text,
                   getSubjectCombination :: Set LDScienceSubject,
                   getSubmissionStatus :: LDSubmissionStatus
                 } deriving (Show, Eq, Data, Typeable, Generic)
instance ToJSON LDStudent
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''LDStudent)

type LDIndexNumber = Int
type LDClass = IntMap LDStudent
type LDClasses = Map LDClassName LDClass

newtype LDStorage = LDStorage { getClasses :: LDClasses } deriving (Show, Data, Typeable, ToJSON)
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''LDStorage)

-- |
-- == Permitted Operations

-- | A wrapper that makes a simple function into a Query monad.
wrapQuery :: (LDClasses -> a) -> Acid.Query LDStorage a
wrapQuery theQuery = liftM (theQuery . getClasses) ask

-- | A wrapper that makes a simple function into an Update monad.
wrapUpdate :: (LDClasses -> (LDClasses, a)) -> Acid.Update LDStorage a
wrapUpdate theUpdate = liftM2 (>>) (put . LDStorage . fst) (return . snd) . theUpdate . getClasses =<< get

-- TODO make all the Maybe in the op* functions Either T.Text

-- | Lists available classes.
opGetClasses :: LDClasses -> Maybe [LDClassName]
opGetClasses = Just . Map.keys
-- TODO should filter out classes without students? without students who haven't completed submission?

-- | Lists all students in a particular class.
opGetAllStudentsInClass :: LDClassName -> LDClasses -> Maybe [(LDIndexNumber, LDStudent)]
opGetAllStudentsInClass className s = do
  klass <- Map.lookup className s
  return $ IntMap.assocs klass

-- | Lists all students in a particular class that did not complete
-- submission.
opGetStudentsWithoutSubmissionInClass :: LDClassName -> LDClasses -> Maybe [(LDIndexNumber, LDStudent)]
opGetStudentsWithoutSubmissionInClass className s = do
  students <- opGetAllStudentsInClass className s
  return $ filter (not . hasSubmitted . getSubmissionStatus . snd) students
  where hasSubmitted LDNotSubmittedYet = False
        hasSubmitted _ = True

-- | Get full information for a particular student.
opGetStudentInfo :: LDClassName -> LDIndexNumber -> LDClasses -> Maybe LDStudent
opGetStudentInfo className indexNumber s = do
  klass <- Map.lookup className s
  IntMap.lookup indexNumber klass

-- | Submit information for a student.
opDoSubmission :: LDSubmissionStatus -> LDClassName -> LDIndexNumber -> LDClasses -> (LDClasses, Maybe ())
opDoSubmission submission className indexNumber = (, Just ()) . updateClasses
  where updateClasses = Map.adjust updateClass className
        updateClass = IntMap.adjust updateStudent indexNumber
        updateStudent student = student { getSubmissionStatus = submission }
        -- TODO consider using updateLookupWithKey to determine whether it has indeed been changed; don't fail silently

-- | Add a new student.
opAddStudent :: LDClassName -> LDIndexNumber -> LDStudent -> LDClasses -> (LDClasses, Maybe ())
opAddStudent className indexNumber student = (, Just ()) . updateClasses
  where updateClasses = Map.insertWith (const updateClass) className defaultClass
        defaultClass = IntMap.singleton indexNumber student
        updateClass = IntMap.insert indexNumber student

#include "TestData.hs"

-- | The query and update functions wrapped in Query and Update
-- monads.
-- TODO investigate using TH to auto-generate things
queryGetClasses = wrapQuery opGetClasses
queryGetAllStudentsInClass = wrapQuery . opGetAllStudentsInClass
queryGetStudentsWithoutSubmissionInClass = wrapQuery . opGetStudentsWithoutSubmissionInClass
queryGetStudentInfo = (wrapQuery .) . opGetStudentInfo
updateDoSubmission = ((wrapUpdate .) .) . opDoSubmission
updateAddStudent = ((wrapUpdate .) .) . opAddStudent

-- | Control structures for making transactions acidic.
$(Acid.makeAcidic ''LDStorage ['queryGetClasses,
                               'queryGetAllStudentsInClass,
                               'queryGetStudentsWithoutSubmissionInClass,
                               'queryGetStudentInfo,
                               'updateDoSubmission,
                               'updateAddStudent])

-- | The embeded static files subsite.
mkEmbeddedStatic False "eStatic" [embedDir "static"]

-- | The main webapp.
data LabDeclarationApp = LabDeclarationApp
                         { getStatic :: EmbeddedStatic,
                           getAcid :: Acid.AcidState LDStorage
                         }

mkYesod "LabDeclarationApp" [parseRoutes|
/static StaticR EmbeddedStatic getStatic
/api/classes ClassesR GET
/api/classes/#LDClassName StudentsR GET
/api/classes/#LDClassName/#LDIndexNumber StudentInfoR GET POST
/ HomepageR GET
|]

instance Yesod LabDeclarationApp where
  addStaticContent = embedStaticContent getStatic StaticR minifym
  errorHandler (InvalidArgs ia) = return . toTypedContent $ object [ "meta" .= object [ "code" .= (400 :: Int), "details" .= ia ] ]
  errorHandler other = defaultErrorHandler other

instance RenderMessage LabDeclarationApp FormMessage where
  renderMessage _ _ = defaultFormMessage

-- |
-- = Handlers
-- The actual handlers that handle requests.

-- |
-- == API Handlers
-- These handlers may receive www-urlencoded data and return JSON
-- response.

-- | A generic handler that interfaces with Acid queries and updates.
acidHandler
  :: (Acid.Method ev, ToJSON a, Acid.MethodResult ev ~ Maybe t,
      Acid.MethodState ev ~ LDStorage) =>
     (Acid.AcidState (Acid.EventState ev) -> ev -> IO (Acid.EventResult ev))
     -> (t -> a) -> ev -> Handler Value
acidHandler action f theAction = do
  acid <- getAcid <$> ask
  result <- lift $ action acid theAction
  let responseEnvelope status = [ "meta" .= object [ "code" .= HTTP.statusCode status ] ]
  case result of
   Nothing -> sendResponseStatus HTTP.status400 $ object $ responseEnvelope HTTP.status400
   Just successResult -> return $ object $ responseEnvelope HTTP.status200 ++ [ "data" .= f successResult ]

getClassesR :: Handler Value
getClassesR = acidHandler Acid.query id QueryGetClasses

getStudentsR :: LDClassName -> Handler Value
getStudentsR = acidHandler Acid.query (map (second getName)) . QueryGetStudentsWithoutSubmissionInClass

getStudentInfoR :: LDClassName -> LDIndexNumber -> Handler Value
getStudentInfoR = (acidHandler Acid.query id .) . QueryGetStudentInfo
-- TODO this is insecure; should filter out submission status based on auth result

postStudentInfoR :: LDClassName -> LDIndexNumber -> Handler Value
postStudentInfoR className indexNumber = do
  request <- runInputPost $ LDSubmitted
             <$> ireq sgPhoneField "phone"
             <*> ireq whatWgEmailField "email"
             <*> ireq sigPngField "signature"
  acidHandler Acid.update id $ UpdateDoSubmission request className indexNumber
  where sgPhoneField = checkBool isValidSgPhone ("Invalid phone number" :: T.Text) textField
        whatWgEmailField = checkBool isValidWhatWgEmail ("Invalid email" :: T.Text) textField
        sigPngField = checkBool isValidPngDataUrl ("Invalid signature" :: T.Text) textField

-- |
-- === Data validation

-- | Tests whether a given text string is a valid email. This uses the
-- validation found in WHATWG HTML standard, not RFC 5322.
isValidWhatWgEmail :: T.Text -> Bool
isValidWhatWgEmail = isValidWhatWgEmail' . encodeUtf8
  where isValidWhatWgEmail' = (isRight .) . PC.parseOnly $ do
          username
          PC.char '@'
          domainPart `PC.sepBy1` PC.char '.'
          PC.endOfInput
        username = PC.takeWhile1 $ PC.inClass "a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-"
        domainPart = do
          r <- PC.takeWhile1 (PC.inClass "a-zA-Z0-9") `PC.sepBy1` PC.char '-'
          guard $ C.length (C.intercalate "-" r) <= 63

-- | Tests whether a given text is a Singaporean phone number, with
-- stringent format requirements.
isValidSgPhone :: T.Text -> Bool
isValidSgPhone = isValidSgPhone' . encodeUtf8
  where isValidSgPhone' = (isRight .) . PC.parseOnly $ do
          PC.string "+65 "
          PC.count 4 PC.digit
          PC.char ' '
          PC.count 4 PC.digit
          PC.endOfInput

-- | Tests whether the submitted data URL is a valid PNG image.
isValidPngDataUrl :: T.Text -> Bool
isValidPngDataUrl t = isRight $ getPngBase64 (encodeUtf8 t) >>= Base64.decode >>= Png.decodePng
  where getPngBase64 = PC.parseOnly $ do
          PC.string "data:image/png;base64,"
          PC.takeByteString
-- |
-- == HTML Handlers
-- These handlers return HTML responses.

-- | The user-facing frontend.
getHomepageR :: Handler Html
getHomepageR = defaultLayout $ do
  setTitle "River Valley High School Science Lab Declaration"
  toWidgetHead $(hamletFile "templates/app.head.hamlet")
  addScript $ StaticR jquery_js
  addScript $ StaticR jquery_mobile_custom_js
  toWidget $(juliusFile "templates/app.julius")
  toWidget $(hamletFile "templates/app.hamlet")
  toWidget $(cassiusFile "templates/app.cassius")

main = bracket acidBegin acidFinally run
  where acidBegin = Acid.openLocalState $ LDStorage ldClassesTestData
        acidFinally = Acid.createCheckpointAndClose
        run acid = toWaiApp (LabDeclarationApp eStatic acid) >>= runLocalhost
        runLocalhost = Warp.runSettings
                       $ Warp.setPort 8080
                       $ Warp.setHost "127.0.0.1"
                       Warp.defaultSettings
