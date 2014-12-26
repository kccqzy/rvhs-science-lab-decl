{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module LabDecl.StudentCSV (
  parseCSV, CsvStudent(..),
  klass, indexNo, nric, name, chinese, subjCombi, witness) where

import Control.Monad
import Control.Monad.State
import Control.Arrow (first, second)
import Control.Error
import Control.Lens
import Data.List
import Data.Ord
import Data.Char
import Data.Function
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.CSV.Conduit as CSV
import Data.Default
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import LabDecl.Utilities
import LabDecl.ErrMsg

data CsvStudent = CsvStudent {
  _klass     :: T.Text,
  _indexNo   :: T.Text,
  _nric      :: T.Text,
  _name      :: T.Text,
  _chinese   :: T.Text,
  _subjCombi :: T.Text,
  _witness   :: T.Text
  } deriving (Show)
$(makeLenses ''CsvStudent)

instance Default T.Text where
  def = T.empty

instance Default CsvStudent where
  def = CsvStudent def def def def def def def

type ColumnNumber = Int
type RowNumber = Int
type CsvStudentField = ALens' CsvStudent T.Text
type HeaderAssoc = (ColumnNumber, CsvStudentField)
type Vector2D a = Vector (Vector a)

-- | Decodes a stream of csv text into a vector of lists.
decodeCSV :: T.Text -> Either TL.Text (Vector2D T.Text)
decodeCSV = note errCSVDecodeFailed . hush . CSV.decodeCSV CSV.defCSVSettings

-- | Use heuristics to find the main table header, ignoring
-- superfluous values, auxiliary tables, titles etc. It returns a
-- sliced vector containing the table with header row and everything
-- preceding the header row stripped away.
heuristicFindTableHeader :: Vector2D T.Text -> Either TL.Text (Vector (RowNumber, Vector T.Text), [HeaderAssoc])
heuristicFindTableHeader rawTable = join . runEitherR $ do
  let indexedTable = V.indexed rawTable
  V.forM_ indexedTable $ \(i, row) -> do
    let canoc = map (T.toCaseFold . T.unwords . T.words . T.strip) $ V.toList row
    let results = sortBy (compare `on` snd) . mapMaybe fromSndMaybe . zip [0..] . map (`Map.lookup` canonicalMap) $ canoc
    let duplicateResults = filter ((>1) . length) . groupBy ((==) `on` snd) $ results
    case duplicateResults of
     (js:_) -> succeed . Left . errCSVDuplicateColumnHeader . intercalate ", " . map ((++show (i+1)) . formatColumnNumber . fst) $ js
     [] -> return ()
    when ([0..6] == map snd results) $ do
      let headers = mapMaybe (fromSndMaybe . second (`IntMap.lookup` lensMap)) results
      let sliced = V.drop (i+1) indexedTable
      succeed $ Right (sliced, headers)
  return (errCSVHeaderNotFound canonicalNames)

-- | Create a raw student from the table rows.
makeCsvStudent :: [HeaderAssoc] -> (RowNumber, Vector T.Text) -> Either TL.Text (RowNumber, CsvStudent)
makeCsvStudent assocs (i, row) = (`runStateT` def) $ do
  forM_ assocs $ \(j, field) -> do
    cell <- lift $ note (errCSVCellNotFound (i+1) (formatColumnNumber j)) (row V.!? j)
    field #= T.strip cell
  return i

parseCSV :: T.Text -> Either TL.Text (Vector (RowNumber, CsvStudent))
parseCSV csvStream = do
  decoded <- decodeCSV csvStream
  (tbody, assocs) <- heuristicFindTableHeader decoded
  let tbody' = V.takeWhile (not . V.all T.null . snd) tbody
  V.mapM (makeCsvStudent assocs) tbody'

canonicalMap :: Map T.Text Int
canonicalMap = Map.fromList . concat . zipWith (\v ks -> [(k, v) | k <- ks]) [0..] . map (map T.toCaseFold . snd) $ expectedColumns

lensMap :: IntMap CsvStudentField
lensMap = IntMap.fromList . zip [0..] . map fst $ expectedColumns

canonicalNames :: T.Text
canonicalNames = T.intercalate ", " . map (head . snd) $ expectedColumns

formatColumnNumber :: Int -> String
formatColumnNumber j | j < 0 = []
                     | otherwise = formatColumnNumber (q-1) ++ [chr (r + ord 'A')]
  where (q,r) = divMod j 26

expectedColumns :: [(CsvStudentField, [T.Text])]
expectedColumns = [
  (klass, ["Class", "C"]),
  (indexNo, ["Index", "Index #", "Index No.", "Index Number", "Register #", "Register No.", "Register Number", "R"]),
  (nric, ["NRIC / FIN", "FIN", "NRIC"]),
  (name, ["Name", "Name (as in NRIC)", "Statutory Name", "Formal Name", "N"]),
  (chinese, ["Chinese Name", "Chinese", "Name in Chinese", "Name in MTL", "Native Name"]),
  (subjCombi, ["Subject Combination", "SC", "Subject Combinations", "Subject", "Subjects", "Subj Combi", "Subj Combination", "Subject Combi"]),
  (witness, ["Witness", "Witnesser", "Witnessing Teacher", "Undertaking Teacher", "Chemistry Teacher", "Teacher", "W", "T"])
  ]
