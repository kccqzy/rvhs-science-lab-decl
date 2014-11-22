module LabDecl.Utilities where

import Control.Monad
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Base64 as C64
import qualified Data.ByteString.Base64.Lazy as CL64
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.Char
import Data.List
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Language.Haskell.TH

unique :: Set a -> Maybe a
unique s = case Set.toList s of
  [a] -> Just a
  _ -> Nothing

embedFile :: FilePath -> Q Exp
embedFile filename = (LitE . stringL) <$> runIO (CL.unpack <$> CL.readFile filename)

mapQ :: Name -> [Name] -> Q Exp
mapQ f = return . ListE . map (AppE (VarE f) . VarE)

camelCaseToUnderScore :: String -> String
camelCaseToUnderScore = concatMap go
  where go c | isUpper c = '_' : [toLower c]
             | otherwise = [c]

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
