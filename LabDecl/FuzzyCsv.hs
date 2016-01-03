{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module LabDecl.FuzzyCsv (
  formatColumnNumber, parseCsv, CsvText(..), CsvError(..),
  ColumnNumber, RowNumber, HeaderAliases) where

import Control.Monad
import Control.Monad.State
import Control.Error
import Data.List
import Control.Lens
import Data.Char
import Data.Function
import Data.String
import qualified Data.Text as T
import qualified Data.CSV.Conduit as CSV
import Data.Monoid
import Data.Default
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as Map

data CsvError = ErrDecodeFailed
              | ErrHeaderNotFound
              | ErrDuplicateColumnHeader RowNumber [[(ColumnNumber, T.Text)]]
              | ErrCellNotFound RowNumber ColumnNumber
              deriving (Show)

newtype CsvText = CsvText {
  unCsvText :: T.Text
  } deriving (Show, IsString)

instance Default CsvText where
  def = CsvText T.empty

data Accessor a = Accessor {
  getTag :: Int,
  getALens' :: ALens' a CsvText
  }

instance Eq (Accessor a) where
  a == b = getTag a == getTag b

instance Ord (Accessor a) where
  compare a b = compare (getTag a) (getTag b)

type ColumnNumber = Int
type RowNumber = Int
type HeaderAliases a = [(ALens' a CsvText, [T.Text])]

type HeaderAliases' a = Map (Accessor a) [T.Text]
type HeaderAssoc a = (ColumnNumber, Accessor a)
type Vector2D a = Vector (Vector a)

-- | Preprocess the header aliases. This basically adds a numeric tag to it and converts to a Map.
preprocessHeaderAliases :: HeaderAliases a -> HeaderAliases' a
preprocessHeaderAliases = Map.fromList . zipWith (\ctr (alens', names) -> (Accessor ctr alens', names)) [0..]

-- | This tries to convert a map of canonical names to list of aliases to a map
-- of aliases to accessors, arranged sequentially.
getCanonicalMap :: HeaderAliases' a -> Map T.Text (Accessor a)
getCanonicalMap aliases = Map.foldrWithKey (\k v r -> r <> Map.fromList ((,k) <$> v)) Map.empty . Map.map (map T.toCaseFold) $ aliases

-- | Decodes a stream of csv text into a 2D vector.
decodeCsv :: T.Text -> Either CsvError (Vector2D T.Text)
decodeCsv = note ErrDecodeFailed . hush . CSV.decodeCSV CSV.defCSVSettings

fromSndMaybe :: (a, Maybe b) -> Maybe (a, b)
fromSndMaybe = liftM2 (<$>) ((,) . fst) snd

-- | Format a column number in a way expected by Excel users, i.e. A, B, ... Z,
-- AA...
formatColumnNumber :: Int -> String
formatColumnNumber j | j < 0 = []
                     | otherwise = formatColumnNumber (q-1) ++ [chr (r + ord 'A')]
  where (q,r) = divMod j 26

-- | Use heuristics to find the main table header, ignoring
-- superfluous values, auxiliary tables, titles etc. It returns a
-- sliced vector containing the table with header row and everything
-- preceding the header row stripped away.
heuristicFindTableHeader :: HeaderAliases' a -> Vector2D T.Text -> Either CsvError (Vector (RowNumber, Vector T.Text), [HeaderAssoc a])
heuristicFindTableHeader headerAliases rawTable = join . runEitherR $ do
  let indexedTable = V.indexed rawTable
  let canonicalMap = getCanonicalMap headerAliases
  V.forM_ indexedTable $ \(i, row) -> do
    -- First, create a canonical version of the row.
    let canoc = map (T.toCaseFold . T.unwords . T.words . T.strip) $ V.toList row

    -- Next, lookup each cell content to find the accessor, add column
    -- numbers to form a tuple, strip away nonexistent keys.
    let results = mapMaybe fromSndMaybe . zip [0..] . map (`Map.lookup` canonicalMap) $ canoc

    -- Find out whether or not any headers are duplicated. Complain if there
    -- are.
    let duplicateResults = filter ((>1) . length) . groupBy ((==) `on` snd) . sortBy (compare `on` snd) $ results

    case duplicateResults of
     [] -> return ()
     _ -> do
       let duplicateHeaderColumns = (liftM . liftM) fst duplicateResults
       let duplicateHeaderCells = (liftM . liftM) (\n -> (n, row V.! n)) duplicateHeaderColumns
       succeed . Left . ErrDuplicateColumnHeader i $ duplicateHeaderCells

    -- Now we are sure there are no *duplicate* results. We need to check
    -- whether they are complete.
    when (length results == Map.size headerAliases) $ do
      let headers = results
      let sliced = V.drop (i+1) indexedTable
      succeed $ Right (sliced, headers)
  return ErrHeaderNotFound

interpretCsvRow :: (Default a) => [(Int, Accessor a)] -> (RowNumber, Vector T.Text) -> Either CsvError (RowNumber, a)
interpretCsvRow assocs (i, row) = (`runStateT` def) $ do
  forM_ assocs $ \(j, field) -> do
    cell <- lift $ note (ErrCellNotFound i j) (row V.!? j)
    getALens' field #= CsvText (T.strip cell)
  return i

parseCsv :: (Default a) => HeaderAliases a -> T.Text -> Either CsvError (Vector (RowNumber, a))
parseCsv aliases csvStream = do
  let aliases' = preprocessHeaderAliases aliases
  decoded <- decodeCsv csvStream
  (tbody, assocs) <- heuristicFindTableHeader aliases' decoded
  let tbody' = V.takeWhile (not . V.all T.null . snd) tbody
  V.mapM (interpretCsvRow assocs) tbody'
