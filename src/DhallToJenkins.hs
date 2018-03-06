{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DhallToJenkins where

import qualified Data.Map               as Map
import           Data.Text.Lazy         (Text)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.IO      as TL
import qualified Dhall
import qualified Dhall.Core             as Expr (Expr (..))

data Agent
  = Any
  | None
  | Label Text
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
        Expr.UnionLit t e _ <- return expr
        case t of
          "none"  -> return None
          "any"   -> return Any
          "label" -> Dhall.extract labelMaker e
          _       -> error "unexpected agent"
      expected =
        Expr.Union
          (Map.fromList
             [ ("none", Expr.Record Map.empty)
             , ("any", Expr.Record Map.empty)
             , ("label", Dhall.expected labelMaker)
             ])
  in Dhall.Type {..}

labelMaker :: Dhall.Type Agent
labelMaker =
  let extract expr = do
        Expr.TextLit builder <- return expr
        return (Label (Builder.toLazyText builder))
      expected = Expr.Text
  in Dhall.Type {..}
