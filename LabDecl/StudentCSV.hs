{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LabDecl.StudentCSV (
  parseCSV, CsvStudent(..), CsvText(..),
  klass, indexNo, nric, name, chinese, subjCombi, witness) where

import Control.Lens
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Default
import Data.Vector (Vector)

import LabDecl.FuzzyCsv
import LabDecl.ErrMsg

data CsvStudent = CsvStudent {
  _klass     :: CsvText,
  _indexNo   :: CsvText,
  _nric      :: CsvText,
  _name      :: CsvText,
  _chinese   :: CsvText,
  _subjCombi :: CsvText,
  _witness   :: CsvText
  } deriving (Show)
$(makeLenses ''CsvStudent)

instance Default CsvStudent where
  def = CsvStudent def def def def def def def

parseCSV :: T.Text -> Either TL.Text (Vector (RowNumber, CsvStudent))
parseCSV csvStream = case parseCsv expectedColumns csvStream of
  Right r -> Right r
  Left ErrDecodeFailed -> Left errCSVDecodeFailed
  Left ErrHeaderNotFound -> Left $ errCSVHeaderNotFound canonicalNames
  Left (ErrCellNotFound i j) -> Left $ errCSVCellNotFound (i+1) (formatColumnNumber j)
  Left (ErrDuplicateColumnHeader i (headers:_)) -> Left . errCSVDuplicateColumnHeader . T.pack . describeCells i $ headers
  Left (ErrDuplicateColumnHeader _ []) -> error "Impossible!"
  where canonicalNames = T.intercalate ", " . map (head . snd) $ expectedColumns
        describeCells i = intercalate ", " . map ((++show (i+1)) . formatColumnNumber . fst)

expectedColumns :: [(ALens' CsvStudent CsvText, [T.Text])]
expectedColumns = [
  (klass, ["Class", "C"]),
  (indexNo, ["Index", "Index #", "Index No.", "Index Number", "Register #", "Register No.", "Register Number", "R"]),
  (nric, ["NRIC / FIN", "FIN", "NRIC"]),
  (name, ["Name", "Name (as in NRIC)", "Statutory Name", "Formal Name", "N"]),
  (chinese, ["Chinese Name", "Chinese", "Name in Chinese", "Name in MTL", "Native Name"]),
  (subjCombi, ["Subject Combination", "SC", "Subject Combinations", "Subject", "Subjects", "Subj Combi", "Subj Combination", "Subject Combi"]),
  (witness, ["Witness", "Witnesser", "Witnessing Teacher", "Undertaking Teacher", "Chemistry Teacher", "Teacher", "W", "T"])
  ]
