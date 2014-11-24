{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
module LabDecl.Models where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Reader
import Control.Monad.State
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
import Data.Vector (Vector)
import qualified Data.Vector as V
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

-- | Unfortunately, Data.Acid requires the final result of an event
-- function have type `Acid.Query st a` and it does not accept
-- something more general than that. But we need something more
-- general because we want to execute this Reader in a StateT. See
-- below for why we need StateT instead of Acid.Update.
makeAcidQuery :: Reader Database a -> Acid.Query Database a
makeAcidQuery query = runReader query <$> ask

-- | An internal query type. It can be converted to acid-compatible
-- form through makeAcidQuery.
type IQuery a = Reader Database a

-- | List entities in a table.
listEntities :: (Ord a) => Lens' Database (IxSetCtr a) -> IQuery (Set a)
listEntities db = searchEntities db []

-- | Generically search for entities fulfilling some criteria.
searchEntities :: (Ord a) => Lens' Database (IxSetCtr a) -> [IxSet a -> IxSet a] -> IQuery (Set a)
searchEntities db crits = toSet . foldr (.) id crits . snd . (^.db) <$> ask

-- | Search for entities that fulfil an equality criterion.
searchEntitiesEq :: (Indexable a, Typeable k, Typeable a, Ord a) => Lens' Database (IxSetCtr a) -> k -> IQuery (Set a)
searchEntitiesEq db prop = searchEntities db [(@= prop)]


listCcas     = listEntities ccaDb
listSubjects = listEntities subjectDb
listTeachers = listEntities teacherDb
listStudents = listEntities studentDb

lookupCcaById                   :: CcaId          -> IQuery (Maybe Cca)
lookupSubjectById               :: SubjectId      -> IQuery (Maybe Subject)
lookupTeacherByEmail            :: Email          -> IQuery (Maybe Teacher)
lookupTeacherByWitnessName      :: T.Text         -> IQuery (Maybe Teacher)
lookupTeacherById               :: TeacherId      -> IQuery (Maybe Teacher)
lookupSubjectByCodeLevel        :: T.Text -> Int  -> IQuery (Maybe Subject)
lookupStudentByClassIndexNumber :: Class -> Int   -> IQuery (Maybe Student)
lookupCcaById                                     = fmap unique . searchEntitiesEq ccaDb
lookupSubjectById                                 = fmap unique . searchEntitiesEq subjectDb
lookupTeacherByEmail                              = fmap unique . searchEntitiesEq teacherDb
lookupTeacherByWitnessName                        = fmap unique . searchEntitiesEq teacherDb
lookupTeacherById                                 = fmap unique . searchEntitiesEq teacherDb
lookupSubjectByCodeLevel code level               = unique <$> searchEntities subjectDb [(@= code), (@= level)]
lookupStudentByClassIndexNumber klass indexNumber = unique <$> searchEntities studentDb [(@= klass), (@= indexNumber)]

listSubjectsByLevel       :: Int       -> IQuery (Set Subject)
listStudentsFromClass     :: Class     -> IQuery (Set Student)
listStudentsFromCca       :: CcaId     -> IQuery (Set Student)
listStudentsWithSubject   :: SubjectId -> IQuery (Set Student)
listStudentsWithWitnesser :: TeacherId -> IQuery (Set Student)
listStudentsByStatus      :: Bool      -> IQuery (Set Student)
listSubjectsByLevel       = searchEntitiesEq subjectDb
listStudentsFromClass     = searchEntitiesEq studentDb
listStudentsFromCca       = searchEntitiesEq studentDb
listStudentsWithSubject   = searchEntitiesEq studentDb
listStudentsWithWitnesser = searchEntitiesEq studentDb
listStudentsByStatus      = searchEntitiesEq studentDb

-- |
-- = Updates

-- | Unfortunately, the Acid.Update monad is implemented as a monad
-- instead of an UpdateT monad transfomer. This is of course
-- understandable from Data.Acid point of view, because they cannot
-- control what inner monad we're stuffing inside the hypothetical
-- UpdateT, and they cannot know how to unwrap the inner monad. But
-- this destroys atomicity in error-handling. `EitherT e (State s) a`
-- will update the state and then return an error. We want the whole
-- state update to fail, so we use an additional wrapper of type
-- `StateT s (Either e) a`.
makeAcidUpdate :: StateT Database (Either TL.Text) () -> Acid.Update Database (Either TL.Text ())
makeAcidUpdate update = either (return . Left) ((>> return (Right ())) . put) . execStateT update =<< get

-- | An internal update type. It can be converted to acid-compatible
-- form through makeAcidUpdate.
type IUpdate = StateT Database (Either TL.Text) ()

-- | Like Data.Acid's builtin liftQuery, this allows you to perform a
-- query within an update.
liftQuery :: IQuery a -> StateT Database (Either TL.Text) a
liftQuery query = runReader query <$> get

-- | Add an entity to a table, incrementing the counter. This is a
-- primitive operation that never fails.
addEntity :: (RecordId a i) => Lens' Database (IxSetCtr a) -> a -> IUpdate
addEntity db a = db %= \(ctr, ixset) ->
  (succ ctr, IxSet.insert (set idField (idConstructor (succ ctr)) a) ixset)

-- | Remove an entity from a table. It is not an error to delete
-- nonexistent entities. This is a primitive operation that never
-- fails.
removeEntity :: (RecordId a i) => Lens' Database (IxSetCtr a) -> a -> IUpdate
removeEntity db a = db._2 %= deleteIx (a ^. idField)

-- | Replace an entity with a new one in a table. It is not an error
-- if the specified entity did not exist. This is a primitive
-- operation that never fails.
replaceEntity :: (RecordId a i) => Lens' Database (IxSetCtr a) -> a -> IUpdate
replaceEntity db a = db._2 %= updateIx (a ^. idField) a

-- | Convert a set of subjects to a mapping between the subject code
-- and subject.
subjectsToMap :: Set Subject -> Map T.Text Subject
subjectsToMap = Set.foldr (\v m -> maybe m (\c -> Map.insert c v m) (v ^. subjectCode)) Map.empty


-- | Add a new CCA to the database. No uniqueness checks necessary
-- because CCAs are looked up only through auto-incremented IDs.
addCca :: Cca -> IUpdate
addCca = addEntity ccaDb

-- | Add a new subject to the database. Subjects are also looked up
-- through subject code, so they must be unique among each
-- level. Subject *names* are *intentionally* not checked for
-- uniqueness. A variant of Sardinas-Patterson algorithm will check
-- all subjects in the level are uniquely decodable, unless the
-- subject is force added.
addSubject :: Bool -> Subject -> IUpdate
addSubject force subj = do
  case subj ^. subjectCode of
   Nothing -> return () -- no need to check anything
   Just code -> unless force . forM_ (subj ^. subjectLevel . to IntSet.toList) $ \level -> do
     subjects <- liftQuery $ listSubjectsByLevel level
     check level code (subj ^. subjectName) (subjectsToMap subjects)
  addEntity subjectDb subj

  where check level code name subjects = do
          -- first check for uniqueness of subject code
          maybe (return ()) (lift . Left . errSubjectAlreadyExists code name) . Map.lookup code $ subjects
          -- then check for decodability
          unless (uniquelyDecodable . Set.insert code . Map.keysSet $ subjects) .
            lift . Left $ errSubjectsNotUniquelyDecodable name level

-- | Add a new teacher to the database. Check for uniqueness of email
-- and witnesser name unless forced.
addTeacher :: Bool -> Teacher -> IUpdate
addTeacher force teacher = do
  unless force $ do
    tryLookup lookupTeacherByEmail teacherEmail teacher errTeacherEmailAlreadyExists
    tryLookup lookupTeacherByWitnessName teacherWitnessName teacher errTeacherWitnessNameAlreadyExists
  addEntity teacherDb teacher
  where tryLookup query field entity errMsg = do
          existing <- liftQuery . query $ entity ^. field
          maybe (return ()) (lift . Left . errMsg entity) existing

-- | Add a new student to the database. Check for uniqueness of class
-- and index number. This is expected to be called when a teacher adds
-- a student, not when the student does submission.
addStudent :: Bool -> Student -> IUpdate
addStudent force student = do
  unless force $ do
    existing <- liftQuery $ liftM2 lookupStudentByClassIndexNumber (^. studentClass) (^. studentIndexNumber) student
    maybe (return ()) (lift . Left . errStudentAlreadyExists student) existing
  addEntity studentDb student

-- | Add many students to the database. If adding one student fails,
-- everything fails, as expected by atomicity.
addStudents :: Bool -> Vector Student -> IUpdate
addStudents = V.mapM_ . addStudent
-- TODO add an extra argument to identify the row of the CSV file.
