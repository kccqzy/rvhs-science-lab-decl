{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module LabDecl.Models where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Reader
import Control.Monad.State
import Control.Error
import Control.Lens
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
listEntities :: (RecordId a i) => IQuery (Set a)
listEntities = searchEntities []

-- | Generically search for entities fulfilling some criteria.
searchEntities :: (RecordId a i) => [IxSet a -> IxSet a] -> IQuery (Set a)
searchEntities crits = toSet . foldr (.) id crits . snd . (^.dbField) <$> ask

-- | Search for entities that fulfil an equality criterion.
searchEntitiesEq :: (RecordId a i, Typeable k) => k -> IQuery (Set a)
searchEntitiesEq prop = searchEntities [(@= prop)]

-- | Search for a unique entity that fulfils an equality criterion.
searchUniqueEntityEq :: (RecordId a i, Typeable k) => k -> IQuery (Maybe a)
searchUniqueEntityEq = fmap unique . searchEntitiesEq

listCcas     :: IQuery (Set Cca)
listSubjects :: IQuery (Set Subject)
listTeachers :: IQuery (Set Teacher)
listStudents :: IQuery (Set Student)
listCcas     = listEntities
listSubjects = listEntities
listTeachers = listEntities
listStudents = redactSignatureDeclaration listEntities

lookupCcaById                   :: CcaId         -> IQuery (Maybe Cca)
lookupSubjectById               :: SubjectId     -> IQuery (Maybe Subject)
lookupTeacherByEmail            :: Email         -> IQuery (Maybe Teacher)
lookupTeacherByWitnessName      :: T.Text        -> IQuery (Maybe Teacher)
lookupTeacherById               :: TeacherId     -> IQuery (Maybe Teacher)
lookupSubjectByCodeLevel        :: T.Text -> Int -> IQuery (Maybe Subject)
lookupStudentById               :: StudentId     -> IQuery (Maybe Student)
lookupStudentByClassIndexNumber :: Class -> Int  -> IQuery (Maybe Student)
lookupCcaById                                     = searchUniqueEntityEq
lookupSubjectById                                 = searchUniqueEntityEq
lookupTeacherByEmail                              = searchUniqueEntityEq
lookupTeacherByWitnessName                        = searchUniqueEntityEq
lookupTeacherById                                 = searchUniqueEntityEq
lookupStudentById                                 = searchUniqueEntityEq
lookupSubjectByCodeLevel code level               = unique <$> searchEntities [(@= code),  (@= level)]
lookupStudentByClassIndexNumber klass indexNumber = unique <$> searchEntities [(@= klass), (@= indexNumber)]

listSubjectsByLevel       :: Int       -> IQuery (Set Subject)
listStudentsFromClass     :: Class     -> IQuery (Set Student)
listStudentsFromCca       :: CcaId     -> IQuery (Set Student)
listStudentsWithSubject   :: SubjectId -> IQuery (Set Student)
listStudentsWithWitnesser :: TeacherId -> IQuery (Set Student)
listStudentsByStatus      :: Bool      -> IQuery (Set Student)
listSubjectsByLevel       = searchEntitiesEq
listStudentsFromClass     = redactSignatureDeclaration . searchEntitiesEq
listStudentsFromCca       = redactSignatureDeclaration . searchEntitiesEq
listStudentsWithSubject   = redactSignatureDeclaration . searchEntitiesEq
listStudentsWithWitnesser = redactSignatureDeclaration . searchEntitiesEq
listStudentsByStatus      = redactSignatureDeclaration . searchEntitiesEq

redactSignatureDeclaration :: (Functor f) => f (Set Student) -> f (Set Student)
redactSignatureDeclaration = fmap $ Set.map $ (studentSubmission . ssSignature .~ Nothing) . (studentSubmission . ssFinalDeclaration .~ Nothing)

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
addEntity :: (RecordId a i) => a -> IUpdate
addEntity a = dbField %= \(ctr, ixset) ->
  (succ ctr, IxSet.insert (set idField (idConstructor (succ ctr)) a) ixset)

-- | Remove an entity from a table. It is not an error to delete
-- nonexistent entities. This is a primitive operation that never
-- fails.
removeEntity :: forall a i. (RecordId a i) => a -> IUpdate
removeEntity a = dbField'._2 %= deleteIx (a ^. idField)
  where dbField' = dbField :: Lens' Database (IxSetCtr a)

-- | Replace an entity with a new one in a table. It is not an error
-- if the specified entity did not exist. This is a primitive
-- operation that never fails.
replaceEntity :: (RecordId a i) => a -> IUpdate
replaceEntity a = dbField._2 %= updateIx (a ^. idField) a


-- | Convert a set of subjects to a mapping between the subject code
-- and subject.
subjectsToMap :: Set Subject -> Map T.Text Subject
subjectsToMap = Set.foldr (\v m -> maybe m (\c -> Map.insert c v m) (v ^. subjectCode)) Map.empty


-- | Add a new CCA to the database. No uniqueness checks necessary
-- because CCAs are looked up only through auto-incremented IDs.
addCca :: Cca -> IUpdate
addCca = addEntity

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
  addEntity subj

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
    tryLookup teacherEmail teacher errTeacherEmailAlreadyExists
    tryLookup teacherWitnessName teacher errTeacherWitnessNameAlreadyExists
  addEntity teacher
  where tryLookup field entity errMsg = do
          existing <- liftQuery $ unique <$> searchEntitiesEq (entity ^. field)
          maybe (return ()) (lift . Left . errMsg entity) existing

-- | Add a new student to the database. Check for uniqueness of class
-- and index number. This is expected to be called when a teacher adds
-- a student, not when the student does submission.
addStudent :: Bool -> Student -> IUpdate
addStudent force student = do
  unless force $ do
    existing <- liftQuery $ liftM2 lookupStudentByClassIndexNumber (^. studentClass) (^. studentIndexNumber) student
    maybe (return ()) (lift . Left . errStudentAlreadyExists student) existing
  addEntity student

-- | Add many students to the database. If adding one student fails,
-- everything fails, as expected by atomicity.
addStudents :: Bool -> Vector Student -> IUpdate
addStudents = V.mapM_ . addStudent
-- TODO add an extra argument to identify the row of the CSV file.

-- | Ensure an entity exists in the database and then do something
-- about it.
ensureExistThen :: forall a i. (RecordId a i) => (a -> IUpdate) -> a -> IUpdate
ensureExistThen = liftM2 (>>) ensureExist
  where ensureExist entity = do
          existing <- liftQuery $ query entity
          lift $ note (errEntityNotExist existing) existing
          return ()
        query :: a -> IQuery (Maybe a)
        query entity = unique <$> searchEntitiesEq (entity ^. idField)

replaceCca     :: Cca     -> IUpdate
replaceSubject :: Subject -> IUpdate
replaceTeacher :: Teacher -> IUpdate
replaceStudent :: Student -> IUpdate
removeCca      :: Cca     -> IUpdate
removeSubject  :: Subject -> IUpdate
removeTeacher  :: Teacher -> IUpdate
removeStudent  :: Student -> IUpdate
replaceCca     = ensureExistThen replaceEntity
replaceSubject = ensureExistThen replaceEntity
replaceTeacher = ensureExistThen replaceEntity
replaceStudent = ensureExistThen replaceEntity
removeCca      = ensureExistThen removeEntity
removeSubject  = ensureExistThen removeEntity
removeTeacher  = ensureExistThen removeEntity
removeStudent  = ensureExistThen removeEntity

-- |
-- = Public (Restricted) Queries and Updates

-- | A public query to list all classes.
publicListClasses :: IQuery (Set Class)
publicListClasses = Set.map redact <$> listStudents
  where redact = (^. studentClass)

-- | A public query to list all students in a class. We only return
-- index number and student name, for privacy reasons.
publicListStudentsFromClass :: Class -> IQuery (Set (Int, T.Text))
publicListStudentsFromClass = fmap (Set.map redact) . listStudentsFromClass
  where redact = liftM2 (,) (^. studentIndexNumber) (^. studentName)

-- | A public query to obtain complete information of a student. NRIC
-- authentication required.
publicLookupStudentByClassIndexNumber :: Class -> Int -> Nric -> IQuery (Maybe Student)
publicLookupStudentByClassIndexNumber klass indexNumber nric = do
  maybeStudent <- lookupStudentByClassIndexNumber klass indexNumber
  return $ do
    student <- maybeStudent
    guard $ nric == student ^. studentNric
    return student

-- | A public update to do submission. We intentionally do not just
-- take the updated fields as arguments, but rather take a full
-- student and then compare. This is more work but it will be worth it
-- (at least I hope so).
publicStudentDoSubmission :: Student -> IUpdate
publicStudentDoSubmission newStudent = do
  maybeStudent <- liftQuery $ lookupStudentById (newStudent ^. studentId)
  validCcas <- liftQuery $ Set.map (^. ccaId) <$> listCcas
  lift . note errInvalidPublicSubmission $ do
    student <- maybeStudent
    guard . not $ (_SubmissionOpen `isn't` (student ^. studentSubmission))
    guard . not $ (_SubmissionCompleted `isn't` (newStudent ^. studentSubmission))
    guard . isJust . join $ newStudent ^? studentSubmission . ssSignature
    guard $ Just Nothing == newStudent ^? studentSubmission . ssFinalDeclaration
    guard $ maybe False (all (`Set.member` validCcas)) (newStudent ^? studentSubmission . ssCca)
    let changedStudent = student & studentSubmission .~ newStudent ^. studentSubmission
    guard $ changedStudent == newStudent
  replaceEntity newStudent


-- ============================================================

-- | The exported update/query event names. These events will be made
-- acidic, and they do not contain type variables.
eventNames = [
    'listCcas,
    'listSubjects,
    'listTeachers,
    'listStudents,
    'lookupCcaById,
    'lookupSubjectById,
    'lookupTeacherByEmail,
    'lookupTeacherByWitnessName,
    'lookupTeacherById,
    'lookupSubjectByCodeLevel,
    'lookupStudentById,
    'lookupStudentByClassIndexNumber,
    'listSubjectsByLevel,
    'listStudentsFromClass,
    'listStudentsFromCca,
    'listStudentsWithSubject,
    'listStudentsWithWitnesser,
    'listStudentsByStatus,
    'addCca,
    'addSubject,
    'addTeacher,
    'addStudent,
    'addStudents,
    'replaceCca,
    'replaceSubject,
    'replaceTeacher,
    'replaceStudent,
    'removeCca,
    'removeSubject,
    'removeTeacher,
    'removeStudent,
    'publicListClasses,
    'publicListStudentsFromClass,
    'publicLookupStudentByClassIndexNumber,
    'publicStudentDoSubmission
    ]
