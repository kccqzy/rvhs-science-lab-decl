{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LabDecl.Models where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Error (runErrorT, throwError, Error, ErrorT)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ask)
import Control.Lens.Type (Lens')
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.Tuple
import Control.Lens.Fold
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
import Data.IxSet as IxSet
import Data.IxSet.Ix (Ix)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Typeable (Typeable)
import qualified Data.Acid as Acid

import LabDecl.Types
import LabDecl.Utilities

-- |
-- = Queries

-- | List entities in a table.
listEntities :: (Ord a) => Lens' Database (IxSetCtr a) -> Acid.Query Database (Set a)
listEntities db = searchEntities db []

-- | Lookup an entity in a table by its key.
lookupEntity :: (Indexable a, Typeable k, Typeable a, Ord a) => Lens' Database (IxSetCtr a) -> k -> Acid.Query Database (Maybe a)
lookupEntity db recId = unique <$> searchEntities db [(@= recId)]

-- | Generically search for entities fulfilling some criteria.
searchEntities :: (Ord a) => Lens' Database (IxSetCtr a) -> [IxSet a -> IxSet a] -> Acid.Query Database (Set a)
searchEntities db crits = toSet . foldr (.) id crits . snd . (^.db) <$> ask

-- | Search for entities that fulfil an equality criterion.
searchEntitiesEq :: (Indexable a, Typeable k, Typeable a, Ord a) => Lens' Database (IxSetCtr a) -> k -> Acid.Query Database (Set a)
searchEntitiesEq db prop = searchEntities db [(@= prop)]


listCcas     = listEntities ccaDb
listSubjects = listEntities subjectDb
listTeachers = listEntities teacherDb
listStudents = listEntities studentDb

lookupCcaById                   :: CcaId     -> Acid.Query Database (Maybe Cca)
lookupSubjectById               :: SubjectId -> Acid.Query Database (Maybe Subject)
lookupTeacherByEmail            :: Email     -> Acid.Query Database (Maybe Teacher)
lookupTeacherByWitnessName      :: T.Text     -> Acid.Query Database (Maybe Teacher)
lookupTeacherById               :: TeacherId -> Acid.Query Database (Maybe Teacher)
lookupSubjectByCodeLevel        :: T.Text -> Int -> Acid.Query Database (Maybe Subject)
lookupStudentByClassIndexNumber :: Class -> Int -> Acid.Query Database (Maybe Student)
lookupCcaById                                     = fmap unique . searchEntitiesEq ccaDb
lookupSubjectById                                 = fmap unique . searchEntitiesEq subjectDb
lookupTeacherByEmail                              = fmap unique . searchEntitiesEq teacherDb
lookupTeacherByWitnessName                        = fmap unique . searchEntitiesEq teacherDb
lookupTeacherById                                 = fmap unique . searchEntitiesEq teacherDb
lookupSubjectByCodeLevel code level               = unique <$> searchEntities subjectDb [(@= code), (@= level)]
lookupStudentByClassIndexNumber klass indexNumber = unique <$> searchEntities studentDb [(@= klass), (@= indexNumber)]

listSubjectsByLevel       :: Int       -> Acid.Query Database (Set Subject)
listStudentsFromClass     :: Class     -> Acid.Query Database (Set Student)
listStudentsFromCca       :: CcaId     -> Acid.Query Database (Set Student)
listStudentsWithSubject   :: SubjectId -> Acid.Query Database (Set Student)
listStudentsWithWitnesser :: TeacherId -> Acid.Query Database (Set Student)
listStudentsByStatus      :: Bool      -> Acid.Query Database (Set Student)
listSubjectsByLevel       = searchEntitiesEq subjectDb
listStudentsFromClass     = searchEntitiesEq studentDb
listStudentsFromCca       = searchEntitiesEq studentDb
listStudentsWithSubject   = searchEntitiesEq studentDb
listStudentsWithWitnesser = searchEntitiesEq studentDb
listStudentsByStatus      = searchEntitiesEq studentDb
