{-# LANGUAGE OverloadedStrings #-}

module PrettyHelper
  ( prettyOutput
  ) where

import           Data.Text.Lazy                        (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

prettyOutput :: (Pretty a) => a -> IO Text
prettyOutput = render . pretty
  where
    render d = return $ renderLazy $ layoutPretty defaultLayoutOptions d
