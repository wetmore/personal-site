module CssVars ( defaultCtx ) where

import Data.Monoid
import Hakyll

defaultCtx :: Context String
defaultCtx = mconcat $ map (uncurry constField) [
    ("fstColor", "#333")
  , ("sndColor", "#40A2F4")
  ]