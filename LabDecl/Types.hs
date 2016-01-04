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
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base64 as C64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.IxSet as IxSet
import Data.IxSet.Ix (Ix)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Default
import Data.Typeable (Typeable)
import Data.Data (Data)
import GHC.Generics (Generic)
import Data.Time.Calendar
import Data.Aeson
import Data.Aeson.TH
import Data.SafeCopy (base, deriveSafeCopy)
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

-- | A better string representation of @Day@.
instance ToJSON Day where
  toJSON = toJSON . show
instance FromJSON Day where
  parseJSON = parseJSON >=> readZ

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
                         _ssInfoHasError :: Bool,
                         _ssFinalDeclarationFilename :: Maybe T.Text,
                         _ssDate :: Day,
                         _ssUserAgent :: T.Text
                         }
                       deriving (Show, Eq, Ord, Data, Typeable, Generic)
$(makeLenses ''StudentSubmission)
$(makePrisms ''StudentSubmission)

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

-- | A table in the database, with the actual IxSet database and a
-- counter for the current ID.
type IxSetCtr a = (Int, IxSet a)

-- | The default table is an empty table.
instance Indexable a => Default (IxSet a) where
  def = empty

-- | The database.
data Database = Database {
  _ccaDb :: IxSetCtr Cca,
  _subjectDb :: IxSetCtr Subject,
  _teacherDb :: IxSetCtr Teacher,
  _studentDb :: IxSetCtr Student
  } deriving (Show, Data, Typeable)
$(makeLenses ''Database)

-- | The default database has empty tables.
instance Default Database where
  def = Database def def def def

-- | There is a bijective mapping between a record type and its ID
-- type. The typeclass also ensures there exists a lens from the
-- record to its ID, and there exists a way to convert an integer to
-- an ID.
class (FromJSON a, ToJSON a, Indexable a, Ord a, Typeable a, Typeable i) => HasPrimaryKey a i | a -> i, i -> a where
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
$(liftM concat . mapM (deriveSafeCopy 0 'base) $ [''ByteString64, ''Phone, ''Email, ''Nric, ''Class, ''CcaId, ''SubjectId, ''TeacherId, ''StudentId, ''Cca, ''Subject, ''Teacher, ''StudentSubmission, ''Student, ''Database])

-- | ToJSON and FromJSON instances for use when returning structured
-- data.
$(liftM concat . mapM (deriveJSON defaultOptions {
  fieldLabelModifier = jsonLabel
  }) $ [''Cca, ''Subject, ''Teacher, ''StudentSubmission, ''Student])

class ToHTTPStatus a where
  toHttpStatus :: a -> HTTP.Status

instance ToHTTPStatus (Maybe a) where
  toHttpStatus = maybe HTTP.status404 (const HTTP.status200)

instance ToHTTPStatus (Set a) where
  toHttpStatus a = if Set.null a then HTTP.status404 else HTTP.status200

instance ToHTTPStatus (Either a b) where
  toHttpStatus a = if isLeft a then HTTP.status400 else HTTP.status200
