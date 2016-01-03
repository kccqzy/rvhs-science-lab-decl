{-# LANGUAGE RecordWildCards #-}

module LabDecl.RNCryptor (Credentials(..), encrypt, decrypt) where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Error (hoistEither, note, hush, runExceptT)
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as PB
import Data.Word (Word8)
import Data.Byteable
import Crypto.Hash
import Crypto.Cipher.AES
import Crypto.PBKDF.ByteString
import System.Entropy (getEntropy)

data RNCOptions = UseKey
                | UsePassword { rncAesSalt :: B.ByteString, rncHmacSalt :: B.ByteString }

data Credentials = Key { credAesKey :: B.ByteString, credHmacKey :: B.ByteString }
                 | Password { credPassword :: B.ByteString }
                 deriving (Show)

instance Byteable RNCOptions where
  toBytes UseKey = B.singleton 0
  toBytes (UsePassword{..}) = B.concat [ B.singleton 1, rncAesSalt, rncHmacSalt ]

data RNCryptor = RNCryptor {
  rncVersion :: Word8,
  rncOptions :: RNCOptions,
  rncIv :: B.ByteString,
  rncCiphertext :: B.ByteString,
  rncHmac :: B.ByteString
  }

instance Byteable RNCryptor where
  toBytes RNCryptor{..} = B.concat [ B.singleton rncVersion, toBytes rncOptions, rncIv, rncCiphertext, rncHmac ]

parseRNC :: PB.Parser RNCryptor
parseRNC = do
  t <- RNCryptor <$> PB.word8 3 <*> parseRNCOptions <*> PB.take 16 <*> PB.takeByteString <*> pure B.empty
  let l = B.length (rncCiphertext t)
  guard $ l >= 32
  let (realCiphertext, realHmac) = B.splitAt (l - 32) (rncCiphertext t)
  return $ t { rncCiphertext = realCiphertext, rncHmac = realHmac }
  where
    parseRNCOptions :: PB.Parser RNCOptions
    parseRNCOptions = do
      option <- PB.anyWord8
      case option of
       0 -> return UseKey
       1 -> UsePassword <$> PB.take 8 <*> PB.take 8
       _ -> mzero

padMessage :: B.ByteString -> B.ByteString
padMessage msg = B.concat [ msg, B.replicate len (fromIntegral len) ]
  where len = 16 - B.length msg `rem` 16

unpadMessage :: B.ByteString -> Maybe B.ByteString
unpadMessage msg = do
  guard . (==0) . (`rem` 16) . B.length $ msg
  let padlen = B.last msg
  let (unpadded, padding) = B.splitAt (B.length msg - fromIntegral padlen) msg
  guard $ constEqBytes padding (B.replicate (fromIntegral padlen) padlen) -- TODO B.replicate is not constant time
  return unpadded

encrypt :: Credentials -> B.ByteString -> IO (Either String B.ByteString)
encrypt credentials plaintext = runExceptT $ do
  (aesKey, hmacKey, credType) <- case credentials of
   Password password -> do
     hoistEither . note "The password cannot be empty." . guard . not . B.null $ password
     aesSalt <- liftIO $ getEntropy 8
     let aesKey = sha1PBKDF2 password aesSalt 10000 32
     hmacSalt <- liftIO $ getEntropy 8
     let hmacKey = sha1PBKDF2 password hmacSalt 10000 32
     return (aesKey, hmacKey, UsePassword aesSalt hmacSalt)
   Key aesKey hmacKey -> do
     hoistEither . note "The AES key must be exactly 32 bytes." . guard . (==32) . B.length $ aesKey
     hoistEither . note "The HMAC key must be exactly 32 bytes." . guard . (==32) . B.length $ hmacKey
     return (aesKey, hmacKey, UseKey)
  iv <- liftIO $ getEntropy 16
  let padded = padMessage plaintext
  let ciphertext = encryptCBC (initAES aesKey) iv padded
  let t = RNCryptor 3 credType iv ciphertext B.empty
  let mac = toBytes (hmac hmacKey (toBytes t) :: HMAC SHA256)
  return $ B.concat [ toBytes t, mac ]

decrypt :: Credentials -> B.ByteString -> Maybe B.ByteString
decrypt credentials message = do
  rnc@RNCryptor{..} <- hush $ PB.parseOnly parseRNC message
  (aesKey, hmacKey) <- case (rncOptions, credentials) of
   (UseKey, Password _) -> mzero
   (UsePassword _ _, Key _ _) -> mzero
   (UsePassword{..}, Password password) -> return (sha1PBKDF2 password rncAesSalt 10000 32,
                                                   sha1PBKDF2 password rncHmacSalt 10000 32)
   (UseKey, Key aesKey hmacKey) -> return (aesKey, hmacKey)
  let receivedHmac = rncHmac
  let computedHmac = toBytes (hmac hmacKey (toBytes (rnc { rncHmac = B.empty })) :: HMAC SHA256)
  guard $ constEqBytes receivedHmac computedHmac
  let decrypted = decryptCBC (initAES aesKey) rncIv rncCiphertext
  unpadMessage decrypted
