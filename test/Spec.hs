{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text.Lazy.IO       (readFile)
import           System.FilePath         (replaceExtension, takeBaseName)
import           Test.Tasty              (TestTree, defaultMain, testGroup)
import           Test.Tasty.Golden       (findByExtension, goldenVsString)

import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text.Lazy          as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Lazy.IO       as LazyTextIO

import           Pretty

main :: IO ()
main = defaultMain =<< goldenTests

jenkinsfileContents :: FilePath -> IO LBS.ByteString
jenkinsfileContents p =
  fmap (LazyText.encodeUtf8 . LazyText.strip) $ (LazyTextIO.readFile p) >>= dhallToJenkinsfile

goldenTests :: IO TestTree
goldenTests = do
  dhallFiles <- findByExtension [".dhall"] "test"
  return $
    testGroup
      "dhall-to-jenkinsfileContents golden tests"
      [ goldenVsString "with constructors" jenkinsfile (jenkinsfileContents dhallFile)
      | dhallFile <- dhallFiles
      , let jenkinsfile = replaceExtension dhallFile ".jenkinsfile"
      ]
