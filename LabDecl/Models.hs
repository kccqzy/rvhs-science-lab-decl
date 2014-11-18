{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LabDecl.Models where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Base64 as C64
import qualified Data.ByteString.Base64.Lazy as CL64
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.IxSet
import Data.IxSet.Ix
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Default
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.TH
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import qualified Data.Acid as Acid

import LabDecl.Utilities

newtype ByteString64 = ByteString64 C.ByteString deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance ToJSON ByteString64 where
    toJSON (ByteString64 bs) = toJSON (T.decodeUtf8 $ C64.encode bs)
instance FromJSON ByteString64 where
    parseJSON o = parseJSON o >>= either fail (return . ByteString64) . C64.decode . T.encodeUtf8

newtype Email     = Email T.Text      deriving (Show, Eq, Ord, Data, Typeable, Generic, ToJSON, FromJSON)
newtype Nric      = Nric T.Text       deriving (Show, Eq, Ord, Data, Typeable, Generic, ToJSON, FromJSON)
newtype Class     = Class (Int, Char) deriving (Show, Eq, Ord, Data, Typeable, Generic, ToJSON, FromJSON)
newtype CcaId     = CcaId Int         deriving (Show, Eq, Ord, Data, Typeable, Generic, ToJSON, FromJSON)
newtype SubjectId = SubjectId Int     deriving (Show, Eq, Ord, Data, Typeable, Generic, ToJSON, FromJSON)
newtype TeacherId = TeacherId Int     deriving (Show, Eq, Ord, Data, Typeable, Generic, ToJSON, FromJSON)
newtype StudentId = StudentId Int     deriving (Show, Eq, Ord, Data, Typeable, Generic, ToJSON, FromJSON)

ixLitField :: (Typeable i, Ord i) => (a -> i) -> Ix a
ixLitField = ixFun . ((:[]) .)

data Cca = Cca {
  ccaId :: CcaId,
  ccaName :: T.Text,
  ccaCategory :: T.Text
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance Indexable Cca where
  empty = ixSet [ ixLitField ccaId ]


data Subject = Subject {
  subjectId :: SubjectId,
  subjectCode :: Maybe T.Text, -- when a subject has no subject code, it will not appear on subject combination list and is thus a compulsory subject
  subjectName :: T.Text,
  subjectIsScience :: Bool,
  subjectLevel :: IntSet -- this subject is for which level
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance Indexable Subject where
  empty = ixSet $ $(mapQ 'ixLitField [ 'subjectId, 'subjectCode, 'subjectIsScience ]) ++ [
    ixFun $ IntSet.toList . subjectLevel
    ]

data Teacher = Teacher {
  teacherId :: TeacherId,
  teacherIsAdmin :: Bool,
  teacherUnit :: T.Text,
  teacherName :: T.Text,
  teacherWitnessName :: T.Text,
  teacherEmail :: Email
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance Indexable Teacher where
  empty = ixSet $ $(mapQ 'ixLitField [ 'teacherId, 'teacherEmail, 'teacherWitnessName ])


data Student = Student {
  studentId :: StudentId,
  studentName :: T.Text,
  studentChineseName :: Maybe T.Text,
  studentPhone :: Maybe T.Text,
  studentEmail :: Maybe Email,
  studentWitnesser :: Maybe (TeacherId),
  studentClass :: Class,
  studentIndexNumber :: Int,
  studentCca :: Maybe (Set CcaId), -- we sacrifice some performance here by not using IntSet
  studentSubjectCombi :: Maybe (Set SubjectId), -- ditto
  studentNric :: T.Text,
  studentSignaturePng :: Maybe ByteString64
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance Indexable Student where
  empty = ixSet $ $(mapQ 'ixLitField [ 'studentId, 'studentClass ]) ++ [
    ixFun $ maybe [] Set.toList . studentCca,
    ixFun $ maybe [] Set.toList . studentSubjectCombi,
    ixFun $ (:[]) . isJust . studentSignaturePng,
    ixFun $ maybeToList . studentWitnesser
    ]


type IxSetCtr a = (Int, IxSet a)

instance Indexable a => Default (IxSet a) where
  def = empty

type CcaDatabase     = IxSetCtr Cca
type SubjectDatabase = IxSetCtr Subject
type TeacherDatabase = IxSetCtr Teacher
type StudentDatabase = IxSetCtr Student

data Database = Database {
  ccaDb :: CcaDatabase,
  subjectDb :: SubjectDatabase,
  teacherDb :: TeacherDatabase,
  studentDb :: StudentDatabase
  } deriving (Data, Typeable)

instance Default Database where
  def = Database def def def def

type DatabaseGetter a = Database -> IxSetCtr a

$(liftM concat . mapM (deriveSafeCopy 0 'base) $ [''ByteString64, ''Email, ''Nric, ''Class, ''CcaId, ''SubjectId, ''TeacherId, ''StudentId, ''Cca, ''Subject, ''Teacher, ''Student, ''Database])

$(liftM concat . mapM (deriveJSON defaultOptions {
  fieldLabelModifier = liftM3 maybe id (((tail . camelCaseToUnderScore) .) . flip drop) (findIndex isUpper)
  }) $ [''Cca, ''Subject, ''Teacher, ''Student])
