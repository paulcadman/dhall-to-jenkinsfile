{-# LANGUAGE OverloadedStrings #-}

module DhallHelper where

import           Data.Monoid     ((<>))
import           System.FilePath

import qualified Data.Text.Lazy  as LazyText
import qualified Dhall

inputFromFilePath :: FilePath -> Dhall.Decoder a -> IO a
inputFromFilePath fileName d = Dhall.inputFile d rootDhallFile
  where
    rootDhallFile =
      if (isRelative fileName)
        then "./" <> (normalise fileName)
        else fileName
