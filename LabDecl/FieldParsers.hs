{-# LANGUAGE OverloadedStrings #-}
module LabDecl.FieldParsers where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Error
import Control.Concurrent.Async
import qualified Data.Char as Char
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Base64 as C64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Attoparsec.ByteString.Char8 as PC
import qualified Codec.Picture.Png as Png
import qualified Codec.Compression.Zlib as Zlib

import LabDecl.Types

validateThing :: PC.Parser () -> T.Text -> Bool
validateThing thingParser = validateThingBS . T.encodeUtf8
  where validateThingBS = (isRight .) . PC.parseOnly $ thingParser

emailParser :: PC.Parser ()
emailParser = do
  void username
  void $ PC.char '@'
  void $ domainPart `PC.sepBy1` PC.char '.'
  PC.endOfInput
    where username = PC.takeWhile1 $ PC.inClass "a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-"
          domainPart = do
            r <- PC.takeWhile1 (PC.inClass "a-zA-Z0-9") `PC.sepBy1` PC.char '-'
            guard $ C.length (C.intercalate "-" r) <= 63

validateEmail :: T.Text -> Bool
validateEmail = validateThing emailParser

sgPhoneParser :: PC.Parser ()
sgPhoneParser = do
  void $ PC.string "+65 "
  void $ PC.count 4 PC.digit
  void $ PC.char ' '
  void $ PC.count 4 PC.digit
  PC.endOfInput

validatePhone :: T.Text -> Bool
validatePhone = validateThing sgPhoneParser

classParser :: PC.Parser Class
classParser = do
  l <- Char.digitToInt <$> PC.digit
  guard $ 1 <= l && l <= 6
  c <- PC.satisfy $ PC.inClass "A-NP-Z"
  PC.endOfInput
  return $ Class (l, c)

parseClass :: C.ByteString -> Either T.Text Class
parseClass = ((note "wrong class format" . hush) .) . PC.parseOnly $ classParser

nricParser :: PC.Parser Nric
nricParser = do
  void $ many (PC.char 'X')
  prefix <- PC.option Nothing (Just <$> PC.satisfy (PC.inClass "SFTG"))
  digits <- PC.many1 PC.digit
  guard $ length digits >= 3 && length digits <= 7
  suffix <- case (prefix, digits) of
    (Just c, [_,_,_,_,_,_,_]) -> do
      let prefixNum = if c `C.elem` "SF" then 0 else 4
      let check = (`rem` 11) . (+ prefixNum) . sum . zipWith (*) [2,7,6,5,4,3,2] . map Char.digitToInt $ digits
      PC.char $ "JZIHGFEDCBAXWUTRQPNMLK" !! (prefixNum `div` 4 * 11 + check)
    _ -> PC.satisfy $ PC.inClass "JZIHGFEDCBAXWUTRQPNMLK"
  PC.endOfInput
  return . Nric . T.pack $ case prefix of
   Nothing -> digits ++ [suffix]
   Just p  -> p : digits ++ [suffix]

parseNric :: C.ByteString -> Either T.Text Nric
parseNric = ((note "wrong nric format" . hush) . ) . PC.parseOnly $ nricParser

-- Note that after it tests decoding, it returns the original bytestring instead
-- of a parsed picture.
parsePngData :: C.ByteString -> Either T.Text CL.ByteString
parsePngData bs = note "invalid image data" . hush $ do
  data64 <- PC.parseOnly (PC.string "data:image/png;base64," *> PC.takeByteString) bs
  d <- C64.decode data64
  void $ Png.decodePng d
  return $ CL.fromStrict d

parseBase64ZlibData :: C.ByteString -> IO (Either T.Text ByteString64)
parseBase64ZlibData bs = fmap (note "cannot process ua") . runMaybeT $ do
  decoded <- hoistMaybe . hush $ C64.decode bs
  task <- liftIO . async . return . CL.toStrict . Zlib.decompress . CL.fromStrict $ decoded -- this conversion to strict is to force decompression to finish
  resultOrException <- liftIO $ waitCatch task
  result <- hoistMaybe $ hush resultOrException
  return . ByteString64 $ result

parseIntList :: (HasPrimaryKey a i) => C.ByteString -> Either T.Text [i]
parseIntList = ((note "wrong id list" . hush) . ) . PC.parseOnly $
               map idConstructor <$> PC.sepBy1 PC.decimal (PC.char ',') <* PC.endOfInput
