{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module DhallToJenkins where

import qualified Data.HashMap.Strict.InsOrd as Map
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.IO          as TL
import qualified Dhall
import qualified Dhall.Core                 as Expr (Expr (..))
import           DhallHelper

data CustomDockerFile = CustomDockerFile
  { dir                 :: Text
  , filename            :: Maybe Text
  , label               :: Maybe Text
  , additionalBuildArgs :: Maybe Text
  } deriving (Show, Dhall.Generic, Dhall.Interpret)

data CustomDockerImage = CustomDockerImage
  { image :: Text
  , label :: Maybe Text
  , args  :: Maybe Text
  } deriving (Show, Dhall.Generic, Dhall.Interpret)

data DockerFile
  = DefaultFile
  | CustomFile CustomDockerFile
  deriving (Show)

data DockerImage
  = DefaultImage Text
  | CustomImage CustomDockerImage
  deriving (Show)

data Docker
  = File DockerFile
  | Image DockerImage
  deriving (Show)

data Agent
  = Any
  | None
  | Label Text
  | Node Text
  | AgentDocker Docker
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
          "none"   -> return None
          "any"    -> return Any
          "label"  -> Dhall.extract (Label <$> Dhall.lazyText) e
          "node"   -> Dhall.extract (Label <$> Dhall.lazyText) e
          "docker" -> Dhall.extract (AgentDocker <$> dockerMaker) e
          _        -> error "unexpected agent"
      expected =
        Expr.Union
          (Map.fromList
             [ ("none", Expr.Record Map.empty)
             , ("any", Expr.Record Map.empty)
             , ("label", Dhall.expected Dhall.lazyText)
             , ("node", Dhall.expected Dhall.lazyText)
             , ("docker", Dhall.expected (AgentDocker <$> dockerMaker))
             ])
  in Dhall.Type {..}

dockerMaker :: Dhall.Type Docker
dockerMaker =
  let extract expr = do
        Expr.UnionLit t e _ <- return expr
        case t of
          "image" -> Dhall.extract (Image <$> dockerImageMaker) e
          "file"  -> Dhall.extract (File <$> dockerFileMaker) e
          _       -> error "unexpected docker"
      expected =
        Expr.Union
          (Map.fromList
             [ ("image", Dhall.expected (Image <$> dockerImageMaker))
             , ("file", Dhall.expected (File <$> dockerFileMaker))
             ])
  in Dhall.Type {..}

dockerFileMaker :: Dhall.Type DockerFile
dockerFileMaker =
  let extract expr = do
        Expr.UnionLit t e _ <- return expr
        case t of
          "default" -> return DefaultFile
          "custom"  -> Dhall.extract (CustomFile <$> Dhall.auto) e
          _         -> error "unexpected dockerfile"
      expected =
        Expr.Union
          (Map.fromList
             [ ("default", Expr.Record Map.empty)
             , ("custom", Dhall.expected (CustomFile <$> Dhall.auto))
             ])
  in Dhall.Type {..}

dockerImageMaker :: Dhall.Type DockerImage
dockerImageMaker =
  let extract expr = do
        Expr.UnionLit t e _ <- return expr
        case t of
          "default" -> Dhall.extract (DefaultImage <$> Dhall.lazyText) e
          "custom"  -> Dhall.extract (CustomImage <$> Dhall.auto) e
          _         -> error "unexpected dockerimage"
      expected =
        Expr.Union
          (Map.fromList
             [ ("default", Dhall.expected Dhall.lazyText)
             , ("custom", Dhall.expected (CustomImage <$> Dhall.auto))
             ])
  in Dhall.Type {..}
