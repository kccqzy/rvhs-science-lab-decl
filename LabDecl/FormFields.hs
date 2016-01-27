{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
module LabDecl.FormFields where

import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Base64.Lazy as CL64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (utctDay, getCurrentTime)
import Yesod.Core
import Yesod.Form hiding (emailField)

import LabDecl.Types
import LabDecl.FieldParsers

-- | Just return the result in the context of checkMMap.
checkMMapOk :: a -> HandlerT site IO (Either T.Text a)
checkMMapOk = return . return

levelField :: (RenderMessage site FormMessage) => Field (HandlerT site IO) IntSet
levelField = checkMMap fw bw $ checkboxesFieldList $ map (liftM2 (,) (T.pack . show) id) [1..6]
  where fw = checkMMapOk . IntSet.fromList
        bw = IntSet.toList

emailField :: (RenderMessage site FormMessage) => Field (HandlerT site IO) Email
emailField = checkMMap fw bw (checkBool validateEmail ("email wrong format" :: T.Text) textField)
  where fw = checkMMapOk . Email
        bw (Email e) = e

sgPhoneField :: (RenderMessage site FormMessage) => Field (HandlerT site IO) Phone
sgPhoneField = checkMMap fw bw (checkBool validatePhone ("phone wrong format" :: T.Text) textField)
  where fw = checkMMapOk . Phone
        bw (Phone p) = p

classField :: (RenderMessage site FormMessage) => Field (HandlerT site IO) Class
classField = checkMMap fw bw textField
  where bw (Class (l, c)) = T.pack $ show l ++ [c]
        fw = return . parseClass . T.encodeUtf8 . T.toUpper . T.strip

nricField :: (RenderMessage site FormMessage) => Field (HandlerT site IO) Nric
nricField = checkMMap fw bw textField
  where bw (Nric s) = s
        fw = return . parseNric . T.encodeUtf8

todayField :: (RenderMessage site FormMessage) => Field (HandlerT site IO) Day
todayField = checkMMap fw bw checkBoxField
  where bw _ = False
        fw _ = do
          today <- liftIO $ utctDay <$> getCurrentTime
          checkMMapOk today

pngField :: (RenderMessage site FormMessage) => Field (HandlerT site IO) CL.ByteString
pngField = checkMMap fw bw textField
  where bw = T.decodeUtf8 . CL.toStrict . CL.append "data:image/png;base64," . CL64.encode
        fw = return . parsePngData . T.encodeUtf8

compressedDataField :: (RenderMessage site FormMessage) => Field (HandlerT site IO) ByteString64
compressedDataField = checkMMap fw bw textField
  where bw = undefined -- XXX
        fw = liftIO . parseBase64ZlibData . T.encodeUtf8

-- This does not check existence of ids. Only used for directly
-- manipulating the referenced entities, not for creating foreign
-- references.
rawIdsField :: (HasPrimaryKey a i, RenderMessage site FormMessage) => Field (HandlerT site IO) [i]
rawIdsField = checkMMap fw bw textField
  where bw = undefined -- XXX
        fw = return . parseIntList . T.encodeUtf8
