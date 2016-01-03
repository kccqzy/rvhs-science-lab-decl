{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LabDecl.EntityCsv (
  parseStudentCsv, CsvStudent(..),
  parseCcaCsv, CsvCca(..),
  parseSubjectCsv, CsvSubject(..),
  parseTeacherCsv, CsvTeacher(..)
  ) where

import Control.Lens
import Data.List
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Default
import Data.Vector (Vector)

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
