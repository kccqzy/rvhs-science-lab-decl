{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module LabDecl.SubjectCodes where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State (get, put, execStateT, StateT)
import Control.Error
import Data.List
import Data.Function
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as PT
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import LabDecl.Types


data HumanFriendlyParseResult = ParseSuccess (Set Subject)
                              | ParseAmbiguous [Set Subject]
                              | ParseIncomplete (T.Text, Set Subject)
                              | ParseNothing T.Text
                              | ParseInternalError
                              deriving (Show, Eq)


-- | Parses a subject code string into a set of subjects in a human friendly manner.
parseSubjectCodeFriendly :: Map T.Text Subject -> T.Text -> HumanFriendlyParseResult
parseSubjectCodeFriendly allSubjects str =
  case execStateT (subjectCodesParser allSubjects) (str, Set.empty) of
   [] -> ParseInternalError -- unreachable
   [(st, su)] | st == str && Set.null su -> ParseNothing st -- single element
   [_] -> ParseInternalError -- unreachable
   possibilities -> -- more than one element: last one is where nothing is parsed
     case filter (T.null . fst) possibilities of
      [] -> ParseIncomplete . head $ sortBy (compare `on` (T.length . fst)) possibilities
      [(_, subjects)] -> ParseSuccess subjects
      completedPossibilities -> ParseAmbiguous $ map snd completedPossibilities

-- | Parses a subject code string into a set of subjects. This is deprecated.
parseSubjectCode :: Map T.Text Subject -> T.Text -> [Set Subject]
parseSubjectCode allSubjects = map snd . filter (T.null . fst) . execStateT (subjectCodesParser allSubjects) . (,Set.empty)

subjectCodesParser :: Map T.Text Subject -> StateT (T.Text, Set Subject) [] ()
subjectCodesParser = void . many . subjectCodeParser
  where subjectCodeParser candidates = do
          (str, parsed) <- get
          (code, subject) <- lift $ Map.toList candidates
          case T.stripPrefix code str of
           Nothing -> mzero
           Just remaining -> put (T.dropWhile (PT.inClass " \t,;.&/\\+") remaining, Set.insert subject parsed)

-- | Use a variant of the Sardinas-Patterson algorithm to find whether a given
-- set of codes is uniquely decodable. This is used when adding new codes.
uniquelyDecodable :: Set T.Text -> Bool
uniquelyDecodable codes = head $ catMaybes $ zipWith test danglingSuffixes danglingSuffixEq
  where codeList = Set.toList codes
        quotientF n d = sort . catMaybes $ [ T.stripPrefix a b | a <- n, b <- d ]
        initialDanglingSuffix = delete T.empty $ nub (quotientF codeList codeList)
        genDanglingSuffix a = sort $ quotientF codeList a `union` quotientF a codeList
        danglingSuffixes = iterate genDanglingSuffix initialDanglingSuffix
        danglingSuffixEq = False : zipWith (==) danglingSuffixes (tail danglingSuffixes)
        test ssi ssEi
          | T.empty `elem` ssi || any (`elem` codeList) ssi = Just False
          | ssEi = Just True
          | otherwise = Nothing
