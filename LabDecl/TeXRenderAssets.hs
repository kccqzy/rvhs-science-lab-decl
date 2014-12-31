{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module LabDecl.TeXRenderAssets (reportTarGz) where

import Control.Applicative ((<$>))
import qualified Language.Haskell.TH as TH
import qualified Data.ByteString.Lazy.Char8 as CL

reportTarGz :: CL.ByteString
reportTarGz = $((TH.LitE . TH.stringL) <$> TH.runIO (CL.unpack <$> CL.readFile "tex-report.tar.gz"))
