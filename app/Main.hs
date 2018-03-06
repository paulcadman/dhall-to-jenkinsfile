module Main where

import           Data.Text.Lazy
import qualified Data.Text.Lazy.IO as LazyText
import           DhallToJenkins
import           Pretty

main :: IO ()
main = do
  source <- LazyText.getContents
  decoded <- dhallToJenkinsfile source
  putStrLn $ unpack decoded
