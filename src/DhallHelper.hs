{-# LANGUAGE OverloadedStrings #-}

module DhallHelper where

import           Data.Monoid     ((<>))
import           System.FilePath

import qualified Data.Text.Lazy  as LazyText
import qualified Dhall

inputFromFilePath :: FilePath -> Dhall.Type a -> IO a
inputFromFilePath fileName t = Dhall.input t (LazyText.pack rootDhallFile)
  where
    rootDhallFile =
      if (isRelative fileName)
        then "./" <> (normalise fileName)
        else fileName
