{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LabDecl.EntityCsv (
  parseStudentCsv, CsvStudent(..)) where

import Control.Lens
import Data.List
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
