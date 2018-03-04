module Main where

import qualified Data.Text.Lazy.IO as LazyText
import DhallToJenkins
import Dhall

main :: IO ()
main = do
  source <- LazyText.getContents
  decoded <- input pipeline source
  putStrLn $ show decoded
