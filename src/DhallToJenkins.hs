{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DhallToJenkins where

import qualified Data.Map   as Map
import qualified Dhall
import qualified Dhall.Core as Expr (Expr (..))

data Agent
  = Any
  | None
  deriving (Show)

data Pipeline = Pipeline
  { agent :: Agent
  } deriving (Show)

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
        Expr.UnionLit t _ _ <- return expr
        case t of
          "none" -> return None
          "any"  -> return Any
          _      -> error "unexpected agent"
      expected =
        Expr.Union
          (Map.fromList
             [("none", Expr.Record Map.empty), ("any", Expr.Record Map.empty)])
  in Dhall.Type {..}
