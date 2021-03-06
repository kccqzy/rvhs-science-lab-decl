{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
module LabDecl.Types where

import Control.Monad
import Control.Lens (Lens', makePrisms, makeLenses, (^.))
import Control.Error
import Data.Function
import Data.Monoid
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Base64 as C64
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.IxSet as IxSet
import Data.IxSet.Ix (Ix)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.HashMap.Strict as HashMap
import Data.Default
import Data.Typeable (Typeable)
import Data.Data (Data)
import GHC.Generics (Generic)
import Data.Time.Calendar
import Data.Aeson
import Data.Aeson.TH
import Data.SafeCopy
import Text.Shakespeare.Text (ToText)
import Web.PathPieces
import qualified Network.HTTP.Types.Status as HTTP

import LabDecl.Utilities

-- |
-- = Data Structures

-- | A newtype wrapper for ByteString that has ToJSON and FromJSON
-- instances. It base64-encodes them when converting to JSON.
newtype ByteString64 = ByteString64 C.ByteString deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance ToJSON ByteString64 where
    toJSON (ByteString64 bs) = toJSON (T.decodeUtf8 $ C64.encode bs)
instance FromJSON ByteString64 where
    parseJSON o = parseJSON o >>= either fail (return . ByteString64) . C64.decode . T.encodeUtf8

-- | A few newtypes to wrap some values to be used as fields in
-- ADT. This is because IxSet requires every index to have a different
-- type.
newtype Phone     = Phone T.Text      deriving (Show, Eq, Ord, Data, Typeable, Generic, ToJSON, FromJSON, ToText)
newtype Email     = Email T.Text      deriving (Show, Eq, Ord, Data, Typeable, Generic, ToJSON, FromJSON, ToText)
newtype Nric      = Nric T.Text       deriving (Show, Eq, Ord, Data, Typeable, Generic, ToJSON, FromJSON, ToText)
newtype Class     = Class (Int, Char) deriving (Show, Eq, Ord, Data, Typeable, Generic, ToJSON, FromJSON, Read)
newtype CcaId     = CcaId Int         deriving (Show, Eq, Ord, Data, Typeable, Generic, ToJSON, FromJSON, PathPiece, Read)
newtype SubjectId = SubjectId Int     deriving (Show, Eq, Ord, Data, Typeable, Generic, ToJSON, FromJSON, PathPiece, Read)
newtype TeacherId = TeacherId Int     deriving (Show, Eq, Ord, Data, Typeable, Generic, ToJSON, FromJSON, PathPiece, Read)
newtype StudentId = StudentId Int     deriving (Show, Eq, Ord, Data, Typeable, Generic, ToJSON, FromJSON, PathPiece, Read)

infix 4 `nricMatch`
nricMatch :: Nric -> Nric -> Bool
nricMatch = (==) `on` lastFour
  where lastFour (Nric a) = T.drop (T.length a - 4) a

-- | Generate an index from the field literal.
ixLitField :: (Typeable i, Ord i) => Lens' a i -> Ix a
ixLitField f = ixFun $ (:[]) . (^. f)

-- | The CCA table.
data Cca = Cca {
  _ccaId :: CcaId,
  _ccaName :: T.Text,
  _ccaCategory :: T.Text
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)
$(makeLenses ''Cca)

-- | Indexed by only the ID.
instance Indexable Cca where
  empty = ixSet [ ixLitField ccaId ]

-- | The subject table.
data Subject = Subject {
  _subjectId :: SubjectId,
  _subjectCode :: Maybe T.Text, -- when a subject has no subject code, it will not appear on subject combination list and is thus a compulsory subject
  _subjectName :: T.Text,
  _subjectIsScience :: Bool,
  _subjectLevel :: IntSet -- this subject is for which level
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)
$(makeLenses ''Subject)

-- | Indexed by the subject ID, subject code (for lookups when
-- importing student info), isScience and subjectLevel (for lookups
-- when generating reports).
instance Indexable Subject where
  empty = ixSet $ $(mapQ 'ixLitField [ 'subjectId, 'subjectCode, 'subjectIsScience ]) ++ [
    ixFun $ IntSet.toList . (^. subjectLevel)
    ]

-- | The teacher table.
data Teacher = Teacher {
  _teacherId :: TeacherId,
  _teacherIsAdmin :: Bool,
  _teacherUnit :: T.Text,
  _teacherName :: T.Text,
  _teacherWitnessName :: T.Text,
  _teacherEmail :: Email
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)
$(makeLenses ''Teacher)

-- | Indexed by ID, email (for lookups during login), witnesser name
-- (for lookups during student import).
instance Indexable Teacher where
  empty = ixSet $ $(mapQ 'ixLitField [ 'teacherId, 'teacherEmail, 'teacherWitnessName ])

-- | The old student submission status.
data StudentSubmissionV0 = SubmissionNotOpenV0
                         | SubmissionOpenV0
                         | SubmissionCompletedV0 {
                           _ssPhoneV0 :: Phone,
                           _ssEmailV0 :: Email,
                           _ssCcaV0 :: Set CcaId,
                           _ssInfoHasErrorV0 :: Bool,
                           _ssFinalDeclarationFilenameV0 :: Maybe T.Text,
                           _ssDateV0 :: Day,
                           _ssUserAgentV0 :: T.Text
                           }

-- | The student submission status. Note that ^? should be used
-- instead of ^. to access the fields because they may not exist,
-- otherwise the value must form a Monoid. Or use
-- Control.Lens.Prism.isn't to test for constructor.
data StudentSubmission = SubmissionNotOpen
                       | SubmissionOpen
                       | SubmissionCompleted {
                         _ssPhone :: Phone,
                         _ssEmail :: Email,
                         _ssCca :: Set CcaId,
                         _ssFinalDeclarationFilename :: Maybe T.Text,
                         _ssDate :: Day,
                         _ssAnalytics :: ByteString64 -- This is a JSON payload.
                         }
                       deriving (Show, Eq, Ord, Data, Typeable, Generic)
$(makeLenses ''StudentSubmission)
$(makePrisms ''StudentSubmission)

instance Migrate StudentSubmission where
  type MigrateFrom StudentSubmission = StudentSubmissionV0
  migrate SubmissionNotOpenV0 = SubmissionNotOpen
  migrate SubmissionOpenV0 = SubmissionOpen
  migrate (SubmissionCompletedV0 p e c _ f d ua) = SubmissionCompleted p e c f d analytics
    where jsonObj = object ["userAgent" .= ua]
          analytics = ByteString64 . CL.toStrict . B.toLazyByteString . fromEncoding . toEncoding $ jsonObj

-- | The student table.
data Student = Student {
  _studentId :: StudentId,
  _studentName :: T.Text,
  _studentChineseName :: T.Text,
  _studentWitnesser :: Maybe TeacherId,
  _studentClass :: Class,
  _studentIndexNumber :: Int,
  _studentSubjectCombi :: Set SubjectId, -- ditto
  _studentNric :: Nric,
  _studentSubmission :: StudentSubmission
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)
$(makeLenses ''Student)

-- | Indexed by ID, class and index number (during lookups), CCAs
-- (during CCA deletes), subjects (during subject deletes), signature
-- status, witnesser (during teacher deletes).
instance Indexable Student where
  empty = ixSet $ $(mapQ 'ixLitField [ 'studentId, 'studentClass, 'studentIndexNumber ]) ++ [
    ixFun $ Set.toList . (^. studentSubmission . ssCca),
    ixFun $ Set.toList . (^. studentSubjectCombi),
    ixFun $ maybeToList . (^. studentWitnesser),
    ixFun $ textIndex True . (^. studentName)
    ]

-- | The data needed to perform an asynchronous build of PDF, etc.
type AsyncInput = (Student, Maybe Teacher, Set Subject, CL.ByteString)

-- | A table in the database, with the actual IxSet database and a
-- counter for the current ID.
type IxSetCtr a = (Int, IxSet a)

-- | The default table is an empty table.
instance Indexable a => Default (IxSet a) where
  def = empty

-- | The database, old version.
data DatabaseV0 = DatabaseV0 {
  _ccaDbV0 :: IxSetCtr Cca,
  _subjectDbV0 :: IxSetCtr Subject,
  _teacherDbV0 :: IxSetCtr Teacher,
  _studentDbV0 :: IxSetCtr Student
  }

-- | The database.
data Database = Database {
  _ccaDb :: IxSetCtr Cca,
  _subjectDb :: IxSetCtr Subject,
  _teacherDb :: IxSetCtr Teacher,
  _studentDb :: IxSetCtr Student,
  _declarationText :: T.Text
  } deriving (Show, Data, Typeable)
$(makeLenses ''Database)

defaultDeclarationText :: T.Text
defaultDeclarationText =
  "All students are to comply with the rules concerning the use of all\n" <>
  "science laboratories, including the Design and Technology workshops. The\n" <>
  "term *laboratory* herein refers to all science laboratories and\n" <>
  "workshops.\n\n" <>
  "(a) I have attended the *Laboratory Briefing* by my science subject\n" <>
  "    teacher and have read and understood the *Science Laboratory Rules*\n" <>
  "    in the *RVHS Student's Handbook*;\n\n" <>
  "(b) I hereby undertake and agree to abide by these rules at all times. I\n" <>
  "    will conduct myself in a responsible manner when using\n" <>
  "    the laboratory.\n\n"

instance Migrate Database where
  type MigrateFrom Database = DatabaseV0
  migrate (DatabaseV0 a b c d) = Database a b c d defaultDeclarationText

-- | The default database has empty tables.
instance Default Database where
  def = Database def def def def defaultDeclarationText

-- | There is a bijective mapping between a record type and its ID
-- type. The typeclass also ensures there exists a lens from the
-- record to its ID, and there exists a way to convert an integer to
-- an ID.
class (ToJSON a, Ord a, Indexable a, Typeable a, Typeable i) => HasPrimaryKey a i | a -> i, i -> a where
  idField :: Lens' a i
  idConstructor :: Int -> i
  idDestructor :: i -> Int
  dbField :: Lens' Database (IxSetCtr a)

instance HasPrimaryKey Cca CcaId where
  idField = ccaId
  idConstructor = CcaId
  idDestructor (CcaId i) = i
  dbField = ccaDb

instance HasPrimaryKey Subject SubjectId where
  idField = subjectId
  idConstructor = SubjectId
  idDestructor (SubjectId i) = i
  dbField = subjectDb

instance HasPrimaryKey Teacher TeacherId where
  idField = teacherId
  idConstructor = TeacherId
  idDestructor (TeacherId i) = i
  dbField = teacherDb

instance HasPrimaryKey Student StudentId where
  idField = studentId
  idConstructor = StudentId
  idDestructor (StudentId i) = i
  dbField = studentDb

-- | SafeCopy instances for use with Acid.
$(liftM concat . mapM (deriveSafeCopy 0 'base) $ [''ByteString64, ''Phone, ''Email, ''Nric, ''Class, ''CcaId, ''SubjectId, ''TeacherId, ''StudentId, ''Cca, ''Subject, ''Teacher, ''StudentSubmissionV0, ''Student, ''DatabaseV0])
$(deriveSafeCopy 1 'extension ''Database)
$(deriveSafeCopy 1 'extension ''StudentSubmission)

-- | ToJSON instances for use when returning structured data.
instance ToJSON Cca where
  toJSON = $(mkToJSON jsonDeriveOptions ''Cca)
instance ToJSON Subject where
  toJSON = $(mkToJSON jsonDeriveOptions ''Subject)
instance ToJSON Teacher where
  toJSON = $(mkToJSON jsonDeriveOptions ''Teacher)
instance ToJSON StudentSubmission where
  toJSON = $(mkToJSON jsonDeriveOptions ''StudentSubmission)
instance ToJSON Student where
  toJSON = removeAnalyticsFromStudent . $(mkToJSON jsonDeriveOptions ''Student)
    where removeAnalyticsFromStudent val =
            case val of
              Object o -> Object (HashMap.adjust removeAnalyticsFromStudentSubmission "submission" o)
              _ -> error "Unexpected JSON serialisation for Student"
          removeAnalyticsFromStudentSubmission val =
            case val of
              Object o -> Object (HashMap.delete "analytics" o)
              _ -> error "Unexpected JSON serialisation for StudentSubmission"

class ToHTTPStatus a where
  toHttpStatus :: a -> HTTP.Status

instance ToHTTPStatus (Maybe a) where
  toHttpStatus = maybe HTTP.status404 (const HTTP.status200)

instance ToHTTPStatus (Set a) where
  toHttpStatus a = if Set.null a then HTTP.status404 else HTTP.status200

instance ToHTTPStatus (Either a b) where
  toHttpStatus a = if isLeft a then HTTP.status400 else HTTP.status200
