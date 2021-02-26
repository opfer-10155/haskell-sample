module Mod.Parent (
  f1,
  f2
) where

import qualified Mod.Sub as Sub

f1 f = length . Sub.f1 f
f2 f = length . Sub.f2 f