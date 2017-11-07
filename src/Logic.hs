module Logic where

import Data.Maybe
import Data.List
import Prelude
import Graphics.UI.Threepenny.Events

type Tile = Maybe Int
type Grid = [[Tile]]

-- example: mergeRow [Just 4, Nothing, Just 2, Just 2] -> [Just 4,Just 4]
mergeRow :: [Tile] -> [Tile]
mergeRow tiles = case tiles of
  Nothing:xs -> mergeRow xs
  x:Nothing:xs -> mergeRow (x:xs)
  (Just x):(Just y):xs -> if x == y then (Just $ x * 2) : (mergeRow xs) else Just x :(mergeRow ((Just y):xs))
  xs -> xs

-- example : leftRow [Just 4, Nothing, Just 2, Just 2] -> [Just 4,Just 4] ++ [Nothing, Nothing]
--                                                     -> [Just 4, Just 4, Nothing, Nothing]
leftRow :: [Tile] -> [Tile]
leftRow t = x ++ replicate (4 - (length x)) Nothing
  where x = mergeRow t

-- call leftRow on each Row of Tiles in Grid
-- example: leftGrid [[Just 2, Just 2, Just 4, Just 4],
--                    [Just 2, Nothing, Just 2, Just 2],
--                    [Just 4, Just 4, Just 4, Just 4],
--                    [Just 4, Just 2, Just 2, Just 2]]

leftGrid :: Grid -> Grid
leftGrid g = map leftRow g

-- keycodes from: https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyCode

move :: KeyCode -> Grid -> Grid
move k g = case k of
  37 -> leftGrid g                             -- arrow left
  39 -> map reverse $ leftGrid (map reverse g) -- arrow right
  38 -> transpose $ leftGrid $ transpose g     -- arrow up
  40 -> transpose $ map reverse $ leftGrid $ map reverse $ transpose g -- arrow down
