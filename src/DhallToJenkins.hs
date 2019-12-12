{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module DhallToJenkins where

import qualified Data.HashMap.Strict.InsOrd as Map
import qualified Dhall
import qualified Dhall.Core                 as Expr (Expr (..))
import           DhallHelper

type Text = Dhall.Text

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

dockerFile :: Dhall.Decoder DockerFile
dockerFile = Dhall.union
  ( ( DefaultFile <$ Dhall.constructor "default" Dhall.unit )
    <> ( CustomFile <$> Dhall.constructor "custom" Dhall.auto )
  )

data DockerImage
  = DefaultImage Text
  | CustomImage CustomDockerImage
  deriving (Show, Dhall.Generic, Dhall.FromDhall)

dockerImage :: Dhall.Decoder DockerImage
dockerImage = Dhall.union
  ( ( DefaultImage <$> Dhall.constructor "default" Dhall.strictText )
  <> ( CustomImage <$> Dhall.constructor "custom" Dhall.auto )
  )

data Docker
  = File DockerFile
  | Image DockerImage
  deriving (Show, Dhall.Generic)

docker :: Dhall.Decoder Docker 
docker = 
  Dhall.union
  ( ( File <$> Dhall.constructor "file" dockerFile )
  <> ( Image <$> Dhall.constructor "image" dockerImage )
  )

data Agent
  = Any
  | None
  | Label Text
  | Node Text
  | AgentDocker Docker
  deriving (Show, Dhall.Generic)

decodeAgent :: Dhall.Decoder Agent 
decodeAgent = Dhall.union
  (  ( Any <$ Dhall.constructor "any" Dhall.unit )
  <> ( None <$ Dhall.constructor "none" Dhall.unit )
  <> ( Label <$> Dhall.constructor "label" Dhall.strictText )
  <> ( Node <$> Dhall.constructor "node" Dhall.strictText )
  <> ( AgentDocker <$> Dhall.constructor "docker" docker )
  )

data Pipeline = Pipeline
  { agent :: Agent
  } deriving (Show, Dhall.Generic)

decoderPipeline :: Dhall.Decoder Pipeline 
decoderPipeline = 
  Dhall.record
    ( Pipeline <$> Dhall.field "agent" decodeAgent 
    )

dhallToJenkinsfile :: FilePath -> IO Pipeline
dhallToJenkinsfile = flip inputFromFilePath decoderPipeline
