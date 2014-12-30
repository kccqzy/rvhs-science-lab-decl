{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module LabDecl.AsyncServ where

import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Base64 as C64
import qualified Data.ByteString.Base64.Lazy as CL64
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Aeson as JSON
import Data.Aeson.TH
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP
import qualified RNCryptor

import LabDecl.Types
import LabDecl.Utilities

import Debug.Trace

data GAEMail = GAEMail {
  mailSender :: T.Text,
  mailTo :: T.Text,
  mailSubject :: T.Text,
  mailBody :: TL.Text,
  mailAttachments :: [(T.Text, ByteString64)]
  } deriving (Show, Eq)

-- | ToJSON and FromJSON instances for use when returning structured
-- data.
$(deriveJSON defaultOptions {
  fieldLabelModifier = jsonLabel
  } ''GAEMail)

-- | Package up an email in the format expected by Python mail daemon.
packageMail :: GAEMail -> IO HTTP.Request
packageMail mail = do
  let encoded = CL.toStrict $ JSON.encode mail
  Right encrypted <- RNCryptor.encrypt credentials encoded
  origRequest <- HTTP.parseUrl url
  let request = HTTP.urlEncodedBody [("payload", C64.encode encrypted)] origRequest
  return request
  where credentials = RNCryptor.Password "TXyT-oqOs-Jwq0-knne-Vui8-1ABd-R1b5-56jL-zhph-LcwL-PzfZ-HIkv-4sLt-AHMK-3LO3-mXEj"
#ifdef DEVELOPMENT
        url = "http://localhost:8080/"
#else
        url = "https://rvhs-sci-lab-undertaking.appspot.com/"
#endif

-- | Sends an email using the Python mailer.
sendMail :: HTTP.Manager -> GAEMail -> IO ()
sendMail manager mail = do
  packagedMail <- packageMail mail
  void $ HTTP.httpNoBody packagedMail manager
