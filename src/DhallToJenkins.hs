{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DhallToJenkins where

import qualified Data.HashMap.Strict.InsOrd as Map
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.IO          as TL
import qualified Dhall
import qualified Dhall.Core                 as Expr (Expr (..))
import           DhallHelper

data Agent
  = Any
  | None
  | Label Text
  | Node Text
  deriving (Show)

data Pipeline = Pipeline
  { agent :: Agent
  } deriving (Show)

dhallToJenkinsfile :: FilePath -> IO Pipeline
dhallToJenkinsfile p = inputFromFilePath p pipeline

pipeline :: Dhall.Type Pipeline
pipeline =
  let extract expr = do
        Expr.RecordLit fields <- return expr
        agent <- Map.lookup "agent" fields >>= Dhall.extract agentMaker
        return Pipeline {..}
      expected =
        Expr.Record (Map.fromList [("agent", Dhall.expected agentMaker)])
  in Dhall.Type {..}

agentMaker :: Dhall.Type Agent
agentMaker =
  let extract expr = do
        Expr.UnionLit t e _ <- return expr
        case t of
          "none"  -> return None
          "any"   -> return Any
          "label" -> Dhall.extract (Label <$> Dhall.lazyText) e
          "node"  -> Dhall.extract (Label <$> Dhall.lazyText) e
          _       -> error "unexpected agent"
      expected =
        Expr.Union
          (Map.fromList
             [ ("none", Expr.Record Map.empty)
             , ("any", Expr.Record Map.empty)
             , ("label", Dhall.expected Dhall.lazyText)
             , ("node", Dhall.expected Dhall.lazyText)
             ])
  in Dhall.Type {..}
