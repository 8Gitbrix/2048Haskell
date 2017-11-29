module Logic where

import Data.Maybe
import Data.List
import Prelude

-- import System.IO (stdin, hReady)
-- import System.IO (stdin, hSetEcho)
-- import Control.Monad (when)
-- import System.Exit


import Control.Monad.IO.Class
import System.Console.Haskeline
-- import qualified System.Random

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


leftGrid :: Grid -> Grid
leftGrid g = map leftRow g



-- startGameRecurs :: R.RandomGen r => (Grid, r) -> IO ()

printTile :: Tile -> String
printTile t = case t of
 Just n -> show n 
 Nothing -> " "

colorRow :: [Tile] -> IO ()
colorRow [] = putStrLn ""
colorRow (x:xs) = do putStr $ "\x1b[32m" ++ (printTile x) ++ " | "
                     colorRow xs

colorGrid :: Grid -> IO ()
colorGrid g = case g of
 [] -> putStrLn ""
 x:xs -> do colorRow x
            colorGrid xs




-- getKey = reverse <$> getKey' ""
--   where getKey' chars = do
--           char <- getChar
--           more <- hReady stdin
--           (if more then getKey' else return) (char:chars)

-- -- Simple menu controller
-- main = do
--   let g =            [[Just 2, Just 2, Just 4, Just 4],
--                    [Just 2, Nothing, Just 2, Just 2],
--                    [Just 4, Just 4, Just 4, Just 4],
--                    [Just 4, Just 2, Just 2, Just 2]]
--   hSetEcho stdin False
--   key <- getKey
--   when (key /= "\ESC") $ do
--     case key of
--       -- "\ESC[A" -> putStr $ colorGrid $ transpose $ leftGrid $ transpose g 
--       -- "\ESC[B" -> putStr $ colorGrid $ transpose $ map reverse $ leftGrid $ map reverse $ transpose g
--       -- "\ESC[C" -> putStr $ colorGrid $ map reverse $ leftGrid (map reverse g)
--       -- "\ESC[D" -> putStr $ colorGrid $ leftGrid g
--       "\ESC[A" -> putStr "UP"
  
--   main


-- keycodes from: https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyCode

-- move :: Int -> Grid -> Grid
-- move k g = case k of
--   37 -> leftGrid g                             -- arrow left
--   39 -> map reverse $ leftGrid (map reverse g) -- arrow right
--   38 -> transpose $ leftGrid $ transpose g     -- arrow up
--   40 -> transpose $ map reverse $ leftGrid $ map reverse $ transpose g -- arrow down

