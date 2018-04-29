{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Pretty
  ( dhallToJenkinsfileOutput
  ) where

import           Data.Maybe                (catMaybes)
import           Data.Text.Lazy            (Text)
import           Data.Text.Prettyprint.Doc
import           DhallToJenkins
import           PrettyHelper

dhallToJenkinsfileOutput :: FilePath -> IO Text
dhallToJenkinsfileOutput p = (dhallToJenkinsfile p) >>= prettyOutput

sbraces :: Doc a -> Doc a
sbraces d = braces (space <> d <> space)

linebraces :: Doc a -> Doc a
linebraces inner = vsep [nest 2 $ vsep [lbrace, inner], rbrace]

keyAndValue :: Pretty b => Doc a -> b -> Doc a
keyAndValue name value = name <+> (squotes $ pretty value)

keyAndMaybeValue :: Pretty b => Doc a -> Maybe b -> Maybe (Doc a)
keyAndMaybeValue name (Just value) = Just (name <+> (squotes $ pretty value))
keyAndMaybeValue name Nothing      = Nothing

instance Pretty Pipeline where
  pretty Pipeline {..} = "pipeline" <+> (linebraces $ pretty agent)

instance Pretty Agent where
  pretty a = "agent" <+> (content a)
    where
      content =
        \case
          Any -> "any"
          None -> "none"
          Label s -> sbraces $ keyAndValue "label" s
          Node s -> sbraces $ "node" <+> (sbraces $ keyAndValue "label" s)
          AgentDocker s -> pretty s

instance Pretty Docker where
  pretty a = content a
    where
      content =
        \case
          File _ -> "file"
          Image s -> pretty s

instance Pretty DockerImage where
  pretty a = linebraces $ "docker" <+> (content a)
    where
      content =
        \case
          DefaultImage s -> squotes $ pretty s
          CustomImage s -> linebraces $ pretty s

instance Pretty CustomDockerImage where
  pretty CustomDockerImage {..} =
    vsep $
    [keyAndValue "image" image] ++
    (catMaybes [keyAndMaybeValue "label" label, keyAndMaybeValue "args" args])
