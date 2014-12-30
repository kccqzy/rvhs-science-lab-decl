{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module LabDecl.TeXRenderAssets (texliveTarGz, reportTarGz) where

import Control.Applicative ((<$>))
import qualified Language.Haskell.TH as TH
import qualified Data.ByteString.Lazy.Char8 as CL

texliveTarGz :: CL.ByteString
texliveTarGz = $((TH.LitE . TH.stringL) <$> TH.runIO (CL.unpack <$> CL.readFile "texlive-2014.x86_64-linux.tar.gz"))

reportTarGz :: CL.ByteString
reportTarGz = $((TH.LitE . TH.stringL) <$> TH.runIO (CL.unpack <$> CL.readFile "tex-report.tar.gz"))
