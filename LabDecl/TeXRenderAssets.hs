{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module LabDecl.TeXRenderAssets (reportAssets) where

import qualified Language.Haskell.TH as TH
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Codec.Archive.Tar as Tar

reportTar :: CL.ByteString
reportTar = $((TH.LitE . TH.stringL) <$> TH.runIO (CL.unpack <$> CL.readFile "tex-report.tar"))

reportAssets :: Tar.Entries Tar.FormatError
reportAssets = Tar.read reportTar
