module Main where

import           Control.Applicative (optional)
import           Data.Text.Lazy
import qualified Data.Text.Lazy.IO   as LazyText
import qualified Options.Applicative as OptParse
import           Pretty

data DhallToJenkinsfileOptions = DhallToJenkinsfileOptions
  { dhallFilePath :: String
  }

dhallToJenkinsfileOptionsParser :: OptParse.Parser DhallToJenkinsfileOptions
dhallToJenkinsfileOptionsParser =
  DhallToJenkinsfileOptions <$>
    (OptParse.argument
       OptParse.str
       (mconcat
          [ OptParse.metavar "<dhall input file>"
          , OptParse.help "The Dhall expression to convert to a Jenkinsfile"
          ]))

main :: IO ()
main = do
  options <- OptParse.execParser opts
  decoded <- dhallToJenkinsfileOutput (dhallFilePath options)
  putStrLn $ unpack decoded
  where
    opts = OptParse.info dhallToJenkinsfileOptionsParser OptParse.fullDesc
