{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
module LabDecl.Models where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put, StateT, execStateT)
import Control.Error
import Control.Lens.Type (Lens')
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.Tuple
import Control.Lens.Fold
import Data.List
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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Typeable (Typeable)
import qualified Data.Acid as Acid

import LabDecl.Types
import LabDecl.Utilities
import LabDecl.ErrMsg

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


-- |
-- = Updates

-- | Add an entity to a table, incrementing the counter. This is a
-- primitive operation that never fails.
addEntity :: (RecordId a i) => Lens' Database (IxSetCtr a) -> a -> EitherT TL.Text (Acid.Update Database) ()
addEntity db a = db %= \(ctr, ixset) ->
  (succ ctr, IxSet.insert (set idField (idConstructor (succ ctr)) a) ixset)

-- | Remove an entity from a table. It is not an error to delete
-- nonexistent entities. This is a primitive operation that never
-- fails.
removeEntity :: (RecordId a i) => Lens' Database (IxSetCtr a) -> a -> EitherT TL.Text (Acid.Update Database) ()
removeEntity db a = db._2 %= deleteIx (a ^. idField)

-- | Replace an entity with a new one in a table. It is not an error
-- if the specified entity did not exist. This is a primitive
-- operation that never fails.
replaceEntity :: (RecordId a i) =>
                 Lens' Database (IxSetCtr a) -> a -> EitherT TL.Text (Acid.Update Database) ()
replaceEntity db a = db._2 %= updateIx (a ^. idField) a

-- | Convert a set of subjects to a mapping between the subject code
-- and subject.
subjectsToMap :: Set Subject -> Map T.Text Subject
subjectsToMap = Set.foldr (\v m -> maybe m (\c -> Map.insert c v m) (v ^. subjectCode)) Map.empty

-- | Add a new CCA to the database. No uniqueness checks necessary
-- because CCAs are looked up only through auto-incremented IDs.
addCca :: Cca -> Acid.Update Database (Either TL.Text ())
addCca = runEitherT . addEntity ccaDb

-- | Add a new subject to the database. Subjects are also looked up
-- through subject code, so they must be unique among each
-- level. Subject *names* are *intentionally* not checked for
-- uniqueness. A variant of Sardinas-Patterson algorithm will check
-- all subjects in the level are uniquely decodable, unless the
-- subject is force added.
addSubject :: Bool -> Subject -> Acid.Update Database (Either TL.Text ())
addSubject force subj = runEitherT $ do
  case subj ^. subjectCode of
   Nothing -> return () -- no need to check anything
   Just code -> unless force . forM_ (subj ^. subjectLevel . to IntSet.toList) $ \level -> do
     subjects <- lift $ Acid.liftQuery $ listSubjectsByLevel level
     check level code (subj ^. subjectName) (subjectsToMap subjects)
  addEntity subjectDb subj

  where check level code name subjects = do
          -- first check for uniqueness of subject code
          maybe (return ()) (left . errSubjectAlreadyExists code name) . Map.lookup code $ subjects
          -- then check for decodability
          unless (uniquelyDecodable . Set.insert code . Map.keysSet $ subjects) $
            left $ errSubjectsNotUniquelyDecodable name level

-- | Add a new teacher to the database. Check for uniqueness of email
-- and witnesser name unless forced.
addTeacher :: Bool -> Teacher -> Acid.Update Database (Either TL.Text ())
addTeacher force teacher = runEitherT $ do
  unless force $ do
    tryLookup lookupTeacherByEmail teacherEmail teacher errTeacherEmailAlreadyExists
    tryLookup lookupTeacherByWitnessName teacherWitnessName teacher errTeacherWitnessNameAlreadyExists
  addEntity teacherDb teacher
  where tryLookup query field entity errMsg = do
          existing <- lift . Acid.liftQuery . query $ entity ^. field
          maybe (return ()) (left . errMsg entity) existing

-- | Add a new student to the database. Check for uniqueness of class
-- and index number. This is expected to be called when a teacher adds
-- a student, not when the student does submission.
addStudent :: Bool -> Student -> EitherT TL.Text (Acid.Update Database) ()
addStudent force student = do
  unless force $ do
    existing <- lift . Acid.liftQuery $ liftM2 lookupStudentByClassIndexNumber (^. studentClass) (^. studentIndexNumber) student
    maybe (return ()) (left . errStudentAlreadyExists student) existing
  addEntity studentDb student

-- | Add a single student through the admin console. The UI will
-- prevent many possible errors and ambiguities so a student object
-- can be constructed.
addOneStudent :: Bool -> Student -> Acid.Update Database (Either TL.Text ())
addOneStudent = (runEitherT .) . addStudent
