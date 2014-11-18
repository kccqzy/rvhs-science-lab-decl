module LabDecl.Utilities where

import Control.Monad
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Char
import Language.Haskell.TH

embedFile :: FilePath -> Q Exp
embedFile filename = (LitE . stringL) <$> runIO (CL.unpack <$> CL.readFile filename)

mapQ :: Name -> [Name] -> Q Exp
mapQ f = return . ListE . map (AppE (VarE f) . VarE)

camelCaseToUnderScore :: String -> String
camelCaseToUnderScore = concatMap go
  where go c | isUpper c = '_' : [toLower c]
             | otherwise = [c]
