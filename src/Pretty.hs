{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Pretty
  ( dhallToJenkinsfileOutput
  ) where

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

instance Pretty Pipeline where
  pretty Pipeline {..} = "pipeline" <+> (linebraces $ pretty agent)

instance Pretty Agent where
  pretty a = "agent" <+> (content a)
    where
      content =
        \case
          Any -> "any"
          None -> "none"
          Label s -> sbraces $ "label" <+> (squotes $ pretty s)
          Node s ->
            sbraces $ "node" <+> (sbraces $ "label" <+> (squotes $ pretty s))
          AgentDocker Docker {..} ->
            linebraces $
            "docker" <+> (linebraces $ "image" <+> (squotes $ pretty image))
