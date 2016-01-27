module LabDecl.Utilities where

import Control.Monad
import Control.Monad.Trans
import Control.Error
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Text as T
import qualified Data.Aeson.TH as JSON
import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Text.ICU.Convert as ICU
import qualified Data.Set as Set
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

lift2 :: (MonadTrans t1, MonadTrans t2, Monad (t1 m), Monad m) => m a -> t2 (t1 m) a
lift2 = lift . lift

textElem :: Char -> T.Text -> Bool
textElem c = isJust . T.find (==c)

tryDecodeEncoding :: String -> C.ByteString -> IO (Maybe T.Text)
tryDecodeEncoding encoding bs = do
  converter <- ICU.open encoding Nothing
  let r = ICU.toUnicode converter bs
  return $ if textElem '\65533' r then Nothing else Just r

tryDecodeAllEncodings :: C.ByteString -> IO (Maybe T.Text)
tryDecodeAllEncodings = foldr1 go . mapM tryDecodeEncoding ("gb18030" : delete "gb18030" ICU.converterNames)
  where go io1 io2 = io1 >>= maybe io2 (return . Just)

textIndex :: Bool -> T.Text -> [T.Text]
textIndex withShort = filter (not . T.null) . nub . concatMap (if withShort then gen else gen3gram) . T.words . T.toCaseFold . T.strip
  where gen3gram p | T.length p <= 3 = [p]
                   | otherwise = T.take 3 p : gen3gram (T.tail p)
        genShort p = [T.take 2 p, T.drop (T.length p - 2) p]
        gen = liftM2 (++) gen3gram genShort

jsonDeriveOptions :: JSON.Options
jsonDeriveOptions = JSON.defaultOptions {
  JSON.fieldLabelModifier = liftM3 maybe id (((tail . camelCaseToUnderScore) .) . flip drop) (findIndex isUpper)
  }
