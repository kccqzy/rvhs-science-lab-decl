{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LabDecl.EntityCsv (
  processStudentCsv,
  processCcaCsv,
  processSubjectCsv,
  processTeacherCsv,
  HasCsvProcessor(..)
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Error
import Control.Lens
import Data.List
import Data.Monoid
import Data.Default
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.Attoparsec.ByteString.Char8 as PC
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Acid as Acid

import LabDecl.Types
import LabDecl.AcidicModels
import LabDecl.AcidicModelInstances
import LabDecl.Models
import LabDecl.FieldParsers
import LabDecl.SubjectCodes
import LabDecl.FuzzyCsv
import LabDecl.ErrMsg

data CsvStudent = CsvStudent {
  _csvStudentKlass, _csvStudentIndexNo, _csvStudentNric, _csvStudentName, _csvStudentChinese, _csvStudentSubjCombi, _csvStudentWitness :: T.Text
  } deriving (Show)
$(makeLenses ''CsvStudent)

instance Default CsvStudent where
  def = CsvStudent T.empty T.empty T.empty T.empty T.empty T.empty T.empty

data CsvCca = CsvCca {
  _csvCcaName, _csvCcaCategory :: T.Text
  } deriving (Show)
$(makeLenses ''CsvCca)

instance Default CsvCca where
  def = CsvCca T.empty T.empty

data CsvTeacher = CsvTeacher {
  _csvTeacherName, _csvTeacherUnit, _csvTeacherWitness, _csvTeacherEmail, _csvTeacherAdmin :: T.Text
  } deriving (Show)
$(makeLenses ''CsvTeacher)

instance Default CsvTeacher where
  def = CsvTeacher T.empty T.empty T.empty T.empty T.empty

data CsvSubject = CsvSubject {
  _csvSubjectCode, _csvSubjectName, _csvSubjectIsScience, _csvSubjectLevel :: T.Text
  } deriving (Show)
$(makeLenses ''CsvSubject)

instance Default CsvSubject where
  def = CsvSubject T.empty T.empty T.empty T.empty

parseGenericCsv :: (Default a) => [(ALens' a T.Text, [T.Text])] -> T.Text -> Either TL.Text (Vector (RowNumber, a))
parseGenericCsv expectedColumns csvStream = case parseCsv expectedColumns csvStream of
  Right r -> Right r
  Left ErrDecodeFailed -> Left errCSVDecodeFailed
  Left ErrHeaderNotFound -> Left $ errCSVHeaderNotFound canonicalNames
  Left (ErrCellNotFound i j) -> Left $ errCSVCellNotFound (i+1) (formatColumnNumber j)
  Left (ErrDuplicateColumnHeader i (headers:_)) -> Left . errCSVDuplicateColumnHeader . T.pack . describeCells i $ headers
  Left (ErrDuplicateColumnHeader _ []) -> error "Impossible!"
  where canonicalNames = T.intercalate ", " . map (head . snd) $ expectedColumns
        describeCells i = intercalate ", " . map ((++show (i+1)) . formatColumnNumber . fst)

isCsvFieldEmpty :: T.Text -> Bool
isCsvFieldEmpty = T.null . T.concat . T.split (`elem` ['-','\8210','\8211','\8212','\65112'])

csvBooleanResponse :: T.Text -> Maybe Bool
csvBooleanResponse response = do
  let trueResponses = T.toCaseFold <$> ["Yes", "Y", "True", "T", "1", "Oui", "Yup", "Yeah"]
  let falseResponses = T.toCaseFold <$> ["No", "N", "False", "F", "0", "Non", "Nope", "Nah", "-", "\8210", "\8211", "\8212", "\65112"]
  case T.toCaseFold response of
    t | t `elem` trueResponses -> return True
    t | t `elem` falseResponses -> return False
    t | T.null t -> return False
    _ -> Nothing

parseStudentCsv :: T.Text -> Either TL.Text (Vector (RowNumber, CsvStudent))
parseStudentCsv = parseGenericCsv [
  (csvStudentKlass, ["Class", "C"]),
  (csvStudentIndexNo, ["Index", "Index #", "Index No.", "Index Number", "Register #", "Register No.", "Register Number", "R"]),
  (csvStudentNric, ["NRIC / FIN", "FIN", "NRIC"]),
  (csvStudentName, ["Name", "Name (as in NRIC)", "Statutory Name", "Formal Name", "N"]),
  (csvStudentChinese, ["Chinese Name", "Chinese", "Name in Chinese", "Name in MTL", "Native Name"]),
  (csvStudentSubjCombi, ["Subject Combination", "SC", "Subject Combinations", "Subject", "Subjects", "Subj Combi", "Subj Combination", "Subject Combi"]),
  (csvStudentWitness, ["Witness", "Witnesser", "Witnessing Teacher", "Undertaking Teacher", "Chemistry Teacher", "Teacher", "W", "T"])
  ]

parseCcaCsv :: T.Text -> Either TL.Text (Vector (RowNumber, CsvCca))
parseCcaCsv = parseGenericCsv [
  (csvCcaName, ["Name", "CCA Name", "N"]),
  (csvCcaCategory, ["Category", "CCA Category", "Cat", "C"])
  ]

parseTeacherCsv :: T.Text -> Either TL.Text (Vector (RowNumber, CsvTeacher))
parseTeacherCsv = parseGenericCsv [
  (csvTeacherName, ["Name", "N"]),
  (csvTeacherUnit, ["Unit"]),
  (csvTeacherWitness, ["Witness Name", "Witness", "Witnesser", "Witnesser Name", "Witnessing Name", "Name for Witness", "Formal Name"]),
  (csvTeacherAdmin, [t b | t <- [id, ("Is "<>), (<>"?"), ("Is "<>) . (<>" ?")], b <- [a n | a <- [id, ("An " <>)], n <- ["Admin", "Administrator"]]]),
  (csvTeacherEmail, ["Email", "Email Address", "Mail", "Email Addr"])
  ]

parseSubjectCsv :: T.Text -> Either TL.Text (Vector (RowNumber, CsvSubject))
parseSubjectCsv = parseGenericCsv [
  (csvSubjectCode, ["Code", "Subject Code"]),
  (csvSubjectName, ["Name", "Subject Name"]),
  (csvSubjectIsScience, [t b | t <- [id, ("Is "<>), (<>"?"), ("Is "<>) . (<>" ?")], b <- ["Science", "Sci"]]),
  (csvSubjectLevel, ["Applies to", "Levels", "Level", "Year", "Years", "Applicable to"])
                                  ]

-- TODO: We should refactor this. Instead of doing this manually, we should have a list of pairs of lens and a function to transform a default entity to the desired one.

processStudentCsv :: MonadIO m => Acid.AcidState Database -> T.Text -> ExceptT TL.Text m (Vector Student)
processStudentCsv acid csvStream = do
    csvData <- hoistEither $ parseStudentCsv csvStream
    -- We first get all subjects and teacher because we need them during
    -- validation.
    allSubjects <- liftIO $ mapM (fmap subjectsToMap . Acid.query acid . ListSubjectsByLevel) [1..6]
    allTeachers <- liftIO $ teachersToMap <$> Acid.query acid ListTeachers
    fmapLT errCSVProcessError . V.forM csvData $ \(rowNumber, CsvStudent{..}) -> do
      -- The rowNumber is 0-indexed.
      name <- if isCsvFieldEmpty _csvStudentName
              then hoistEither . Left . (rowNumber,) $ errGenericObjectNoProperty "student" "name"
              else return $ T.toTitle _csvStudentName
      klass@(Class (level, _)) <- hoistEither . parseClassInCsv rowNumber . T.encodeUtf8 $ _csvStudentKlass
      indexNo <- hoistEither . parseIndexInCsv rowNumber . T.encodeUtf8 $ _csvStudentIndexNo
      nric <- hoistEither . parseNricInCsv rowNumber . T.encodeUtf8 $ _csvStudentNric
      witness <- hoistEither . parseWitnessNameInCsv rowNumber allTeachers $ _csvStudentWitness
      let chinese = if isCsvFieldEmpty _csvStudentChinese then T.empty else _csvStudentChinese
      subjectIds <- if isCsvFieldEmpty _csvStudentSubjCombi
                    then return Set.empty
                    else hoistEither . fmapL (rowNumber,) $ explainParseSubjectCode (allSubjects !! (level-1)) _csvStudentSubjCombi
      return $ Student (StudentId 0) name chinese witness klass indexNo subjectIds nric SubmissionNotOpen
  where parseClassInCsv row bs = note ((row,) . errStudentClassNoParse $ T.decodeUtf8 bs) . hush $ parseClass bs
        parseNricInCsv row bs = note ((row,) . errStudentNricNoParse $ T.decodeUtf8 bs) . hush $ parseNric bs
        parseIndexInCsv row bs = note ((row,) . errStudentIndexNumNoParse $ T.decodeUtf8 bs) . hush $ PC.parseOnly (PC.decimal <* PC.endOfInput) bs
        parseWitnessNameInCsv row teacherMap s =
          case Map.lookup s teacherMap of
           Just t -> return . Just $ t ^. idField
           Nothing -> if isCsvFieldEmpty s then return Nothing else Left (row, errStudentWitnessNoParse s)

processSubjectCsv :: Monad m => T.Text -> ExceptT TL.Text m (Vector Subject)
processSubjectCsv csvStream = do
  csvData <- hoistEither $ parseSubjectCsv csvStream
  fmapLT errCSVProcessError . V.forM csvData $ \(rowNumber, CsvSubject{..}) -> do
      name <- if isCsvFieldEmpty _csvSubjectName
              then hoistEither . Left . (rowNumber,) $ errGenericObjectNoProperty "subject" "name"
              else return $ T.toTitle _csvSubjectName
      let code = if isCsvFieldEmpty _csvSubjectCode then Nothing else Just $ T.toUpper _csvSubjectCode
      isScience <- hoistEither . note ((rowNumber,) $ errSubjectIsScienceNoParse _csvSubjectIsScience) $ csvBooleanResponse _csvSubjectIsScience
      level <- hoistEither . liftM IntSet.fromList . parseLevelsInCsv rowNumber . T.encodeUtf8 $ _csvSubjectLevel
      return $ Subject (SubjectId 0) code name isScience level
  where parseLevelsInCsv row bs = note ((row,) $ errSubjectLevelNoParse (T.decodeUtf8 bs)) . hush $
          PC.parseOnly (PC.sepBy1' PC.decimal (PC.char ',' *> PC.skipSpace) <* PC.endOfInput) bs

processTeacherCsv :: Monad m => T.Text -> ExceptT TL.Text m (Vector Teacher)
processTeacherCsv csvStream = do
  csvData <- hoistEither $ parseTeacherCsv csvStream
  fmapLT errCSVProcessError . V.forM csvData $ \(rowNumber, CsvTeacher{..}) -> do
      when (isCsvFieldEmpty _csvTeacherName) . hoistEither . Left . (rowNumber,) $ errGenericObjectNoProperty "teacher" "name"
      when (isCsvFieldEmpty _csvTeacherUnit) . hoistEither . Left . (rowNumber,) $ errGenericObjectNoProperty "teacher" "unit"
      when (isCsvFieldEmpty _csvTeacherWitness) . hoistEither . Left . (rowNumber,) $ errGenericObjectNoProperty "teacher" "witness name"
      unless (validateEmail _csvTeacherEmail) . hoistEither . Left . (rowNumber,) $ errTeacherEmailNoParse _csvTeacherEmail
      isAdmin <- hoistEither . note ((rowNumber,) $ errTeacherAdminNoParse _csvTeacherAdmin) $ csvBooleanResponse _csvTeacherAdmin
      return $ Teacher (TeacherId 0) isAdmin _csvTeacherUnit _csvTeacherName _csvTeacherWitness (Email _csvTeacherEmail)

processCcaCsv :: Monad m => T.Text -> ExceptT TL.Text m (Vector Cca)
processCcaCsv csvStream = do
  csvData <- hoistEither $ parseCcaCsv csvStream
  fmapLT errCSVProcessError . V.forM csvData $ \(rowNumber, CsvCca{..}) -> do
      when (isCsvFieldEmpty _csvCcaName) . hoistEither . Left . (rowNumber,) $ errGenericObjectNoProperty "CCA" "name"
      when (isCsvFieldEmpty _csvCcaCategory) . hoistEither . Left . (rowNumber,) $ errGenericObjectNoProperty "CCA" "category"
      return $ Cca (CcaId 0) _csvCcaName _csvCcaCategory

class HasCRUDEvents a i le re ae ase => HasCsvProcessor a i le re ae ase where
  csvProcessor :: forall m. MonadIO m => Acid.AcidState Database -> T.Text -> ExceptT TL.Text m (Vector a)

instance HasCsvProcessor Cca CcaId LookupCcaById RemoveCca AddCca AddCcas where
  csvProcessor = const processCcaCsv

instance HasCsvProcessor Subject SubjectId LookupSubjectById RemoveSubject AddSubject AddSubjects where
  csvProcessor = const processSubjectCsv

instance HasCsvProcessor Teacher TeacherId LookupTeacherById RemoveTeacher AddTeacher AddTeachers where
  csvProcessor = const processTeacherCsv

instance HasCsvProcessor Student StudentId LookupStudentById RemoveStudent AddStudent AddStudents where
  csvProcessor = processStudentCsv
