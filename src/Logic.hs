module Logic where

import Data.Maybe (isJust, fromJust)
import Data.List (nub, nubBy)
import Graphics.UI.Threepenny.Events


data Tile = Maybe Int
data Grid = [[Tile]]



move :: KeyCode -> Grid -> Grid
move k = case k of
  37 -> error "define me" -- arrow left
  39 -> error "define me" -- arrow right
  38 -> error "define me" -- arrow up
  40 -> error "define me" -- arrow down
