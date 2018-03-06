{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Pretty
  ( dhallToJenkinsfile
  ) where

import           Data.Text.Lazy                        (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Dhall
import           DhallToJenkins

sbraces :: Doc a -> Doc a
sbraces d = braces (space <> d <> space)

linebraces :: Doc a -> Doc a
linebraces inner = vsep [nest 2 $ vsep [lbrace, inner], rbrace]

dhallToJenkinsfile :: Text -> IO Text
dhallToJenkinsfile t = do
  doc <- input pipeline t
  return $ render (pretty doc)
  where
    render = renderLazy . layoutPretty defaultLayoutOptions

instance Pretty Pipeline where
  pretty Pipeline {..} = "pipeline" <+> (linebraces $ pretty agent)

instance Pretty Agent where
  pretty a = "agent" <+> (content a)
    where
      content =
        \case
          Any -> "any"
          None -> "none"
          Label s -> sbraces $ "label" <+> (squotes (pretty s))
