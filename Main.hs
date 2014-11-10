{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Text as T
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
-- or biology a.k.a. biomedical science.
data LDScienceSubject = LDScience
                      | LDPhysics
                      | LDChemistry
                      | LDBiology
                      deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance ToJSON LDScienceSubject
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''LDScienceSubject)

-- | All the information submitted through the UI: phone, email and
-- the submitted signature.
data LDSubmissionStatus = LDNotSubmittedYet
                        | LDSubmitted
                          { getSubmittedPhone :: Int,
                            getSubmittedEmail :: T.Text,
                            getSubmittedSignature :: T.Text
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
wrapUpdate :: (LDClasses -> LDClasses) -> Acid.Update LDStorage ()
wrapUpdate theUpdate = get >>= put . LDStorage . theUpdate . getClasses

-- | Lists available classes.
opGetClasses :: LDClasses -> [LDClassName]
opGetClasses = Map.keys

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
opDoSubmission :: LDSubmissionStatus -> LDClassName -> LDIndexNumber -> LDClasses -> LDClasses
opDoSubmission submission className indexNumber = updateClasses
  where updateClasses = Map.adjust updateClass className
        updateClass = IntMap.adjust updateStudent indexNumber
        updateStudent student = student { getSubmissionStatus = submission }
        -- TODO consider using updateLookupWithKey to determine whether it has indeed been changed; don't fail silently

-- | Add a new student.
opAddStudent :: LDClassName -> LDIndexNumber -> LDStudent -> LDClasses -> LDClasses
opAddStudent className indexNumber student = updateClasses
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
queryGetStudentInfo = ((.).(.)) wrapQuery opGetStudentInfo
updateDoSubmission = ((.).(.).(.)) wrapUpdate opDoSubmission
updateAddStudent = ((.).(.).(.)) wrapUpdate opAddStudent

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
/api/classes/#LDClassName/#LDIndexNumber StudentInfoR GET
|]

instance Yesod LabDeclarationApp where
  addStaticContent = embedStaticContent getStatic StaticR minifym

instance RenderMessage LabDeclarationApp FormMessage where
  renderMessage _ _ = defaultFormMessage

query :: (Acid.QueryEvent ev, Acid.MethodState ev ~ LDStorage) => ev -> Handler (Acid.EventResult ev)
query theQuery = do
  acid <- getAcid <$> ask
  lift $ Acid.query acid theQuery

getClassesR :: Handler Value
getClassesR = do
  classes <- query QueryGetClasses
  return $ object [ "classes" .= classes ]

getStudentsR :: LDClassName -> Handler Value
getStudentsR className = do
  let request = [ "class" .= className ]
  result <- query $ QueryGetStudentsWithoutSubmissionInClass className
  case result of
   Nothing -> sendResponseStatus HTTP.status404 $ object request
   Just students -> return . object $ request ++ [ "students" .= map (second getName) students ]

getStudentInfoR :: LDClassName -> LDIndexNumber -> Handler Value
getStudentInfoR className indexNumber = do
  let request = [ "class" .= className, "indexNumber" .= indexNumber ]
  result <- query $ QueryGetStudentInfo className indexNumber
  case result of
   Nothing -> sendResponseStatus HTTP.status404 $ object request
   Just student -> return . object $ request ++ [ "student" .= student ]

main = bracket acidBegin acidFinally run
  where acidBegin = Acid.openLocalState $ LDStorage ldClassesTestData
        acidFinally = Acid.createCheckpointAndClose
        run acid = toWaiApp (LabDeclarationApp eStatic acid) >>= runLocalhost
        runLocalhost = Warp.runSettings
                       $ Warp.setPort 8080
                       $ Warp.setHost "127.0.0.1"
                       Warp.defaultSettings
