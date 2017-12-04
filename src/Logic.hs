{-# LANGUAGE TemplateHaskell #-}

module Logic where

import Data.Maybe
import Data.List
import Prelude

import System.Random
import Control.Monad.IO.Class
import System.IO.Unsafe

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|))
import qualified Data.Sequence as S
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

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

leftGrid :: Grid -> Grid
leftGrid g = map leftRow g

printTile :: Tile -> String
printTile t = case t of
 Just n -> show n
 Nothing -> " "

colorRow :: [Tile] -> IO ()
colorRow [] = putStrLn ""
colorRow (x:xs) = do putStr $ (printTile x)
-- colorRow (x:xs) = do putStr $ "\x1b[32m" ++ (printTile x) ++ " | "
                     colorRow xs

colorGrid :: Grid -> IO ()
colorGrid g = case g of
 [] -> putStrLn ""
 x:xs -> do colorRow x
            colorGrid xs

-- updateGrid :: Grid -> Grid
-- updateGrid g = do

scoreGrid :: Grid -> Int -> Int
scoreGrid [] n = n
scoreGrid (x:xs) n = do
                     let temp = scoreRow x 0
                     if temp > n then scoreGrid xs temp
                     else scoreGrid xs n


scoreRow :: [Tile] -> Int -> Int
scoreRow [] n = n
scoreRow (x:xs) n = case x of
 Nothing -> scoreRow xs n
 Just a -> if a > n then scoreRow xs a
           else scoreRow xs n

-- terminalstateCheck :: Grid -> Int
-- terminalstateCheck [] = 0
-- terminalStateCheck g = do
--  let score = scoreGrid g
--  case score of
--   0 ->


-- input should be in the form of a filled board
stuckCheck :: Grid -> Bool
stuckCheck g = do
  case g of
     [[Just a,Just b, Just c, Just d],
      [Just e,Just f, Just g, Just h],
      [Just i,Just j, Just k, Just l],
      [Just m,Just n, Just o, Just p]] -> if (a == b || b == c || c == d ||
                                              e == f || f == g || g == h ||
                                              i == j || j == k || k == l ||
                                              m == n || n == o || o == p ||
                                              a == e || e == i || i == m ||
                                              b == f || f == j || j == n ||
                                              c == g || g == k || k == o ||
                                              d == h || h == l || l == p) then False else True
  -- let other = map reverse $ leftGrid (map reverse g)
  -- let num = 0
  -- case g of
  --   (x:xs) -> (stuckRow x) + stuckRow xs
  --               _ -> 5



-- stuckRow :: [Tile] -> Int
-- stuckRow row = do
--   case row of
--     [Just a, Just b, Just c, Just d] -> if (a == b || b == c || c == d) then 1 else 0
checkFull :: Grid -> Bool
checkFull g = case g of
  [] -> True
  (x:xs) -> checkRowFull x && checkFull xs

checkRowFull :: [Tile] -> Bool
checkRowFull r = case r of
  [] -> True
  (x:xs) -> checkTileExists x && checkRowFull xs

checkTileExists :: Tile -> Bool
checkTileExists t = case t of
  Nothing -> False
  _ -> True

insertRandomTile :: Grid -> Grid
insertRandomTile g = case g of
   (x:xs) -> if checkFull (x:xs) then (x:xs)
             else do
                if checkRowFull x then x:(insertRandomTile xs)
                else do
                   if checkFull xs then (insertRandomTileInRow x):xs
                   else do
                      let (tempNum) = unsafePerformIO $ getStdRandom $ randomR (1,10)
                      if (tempNum::Int) <= 6 then x:(insertRandomTile xs)
                      else (insertRandomTileInRow x):xs

insertRandomTileInRow :: [Tile] -> [Tile]
insertRandomTileInRow  r = case r of
   (x:xs) -> if checkTileExists x then x:(insertRandomTileInRow xs)
             else do
               if checkRowFull xs then (makeRandomTile):xs
               else do
                   let (tempNum) = unsafePerformIO $ getStdRandom $ randomR (1,10)
                   if (tempNum::Int) <= 6 then x:(insertRandomTileInRow xs)
                    else (makeRandomTile):xs

makeRandomTile :: Tile
makeRandomTile = do
   let (tempNum) = unsafePerformIO $ getStdRandom $ randomR (1,10)
   if (tempNum::Int) < 10 then Just 2 else Just 4
-- change to not equals to optimize

findBestMove :: Grid -> Int
findBestMove g = do
   let up = insertRandomTile $ transpose $ leftGrid $ transpose g
   let down = insertRandomTile $ transpose $ map reverse $ leftGrid $ map reverse $ transpose g
   let right = insertRandomTile $ map reverse $ leftGrid (map reverse g)
   let left = insertRandomTile $ leftGrid g

   let upScore = randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up+ randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up
   let downScore =  randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down  + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down  + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down  + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down
   let rightScore = randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right
   let leftScore =  randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left  + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left  + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left  + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left

   let maxScore = maximum [upScore,downScore,rightScore,leftScore]
   if maxScore == upScore then 1
   else if maxScore == downScore then 2
       else if maxScore == rightScore then 3
            else if maxScore == leftScore then 4
            else 4

average :: [Int] -> Int
average xs = (sum xs) `div` (length xs)

oneBestMove :: Grid -> Grid
oneBestMove g = do
    case (findBestMove g) of
       1 -> insertRandomTile $ transpose $ leftGrid $ transpose g
       2 -> insertRandomTile $ transpose $ map reverse $ leftGrid $ map reverse $ transpose g
       3 -> insertRandomTile $ map reverse $ leftGrid (map reverse g)
       4 -> insertRandomTile $ leftGrid g

monteCarloPlayBoard :: Grid -> IO ()
monteCarloPlayBoard g = do
   if checkFull g && stuckCheck g then colorGrid g
   else monteCarloPlayBoard $ oneBestMove g

randomlyPlayBoard :: Grid -> Int
randomlyPlayBoard g = do
   if checkFull g && stuckCheck g then scoreGrid g 0
   else randomlyPlayBoard $ oneMove g

randomlyPlayTilWin :: Int -> Grid -> Int
randomlyPlayTilWin n g = do
   if randomlyPlayBoard g == 2048 then n
   else if n == 10000 then n
        else randomlyPlayTilWin (n+1) g

oneMove :: Grid -> Grid
oneMove g = do
    let (randomDirection) = unsafePerformIO $ getStdRandom $ randomR (1,4)
    if (randomDirection::Int) == 1 then insertRandomTile $ transpose $ leftGrid $ transpose g
    else if (randomDirection::Int) == 2 then insertRandomTile $ transpose $ map reverse $ leftGrid $ map reverse $ transpose g
         else if (randomDirection::Int) == 3 then insertRandomTile $ map reverse $ leftGrid (map reverse g)
              else if (randomDirection::Int) == 4 then insertRandomTile $ leftGrid g
                   else g


--- Game definitions: --
-- Game State:
data Game = Game
  { _grid  :: Grid
  , _score :: Int
  , _dead  :: Bool
  } deriving (Eq, Show)

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Eq, Show)

makeLenses ''Game

-- add options for bot later
initGame :: IO Game
initGame = do
  pure $
    Game { _grid = [[Just 2, Just 2, Nothing, Nothing],
                    [Nothing, Nothing, Nothing, Nothing],
                    [Nothing, Nothing, Nothing, Nothing],
                    [Nothing, Nothing, Nothing, Nothing]]
        , _score = 0
        , _dead = False
        }

{-
primaryLoop :: Grid -> IO ()
primaryLoop g = do
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    colorGrid g
    let num = scoreGrid g 0
    if (checkFull g) && (stuckCheck g) then do
        putStrLn "Stuck. Your score is: "
        print num
        return ()
    else do
        case num of
            2048 -> do
               putStrLn "YOU WIN"
               return ()
            _ -> do

               -- print num
               c <- getLine
               case c of
                    "w" -> primaryLoop  $ insertRandomTile $ transpose $ leftGrid $ transpose g
                    "s" -> primaryLoop  $ insertRandomTile $ transpose $ map reverse $ leftGrid $ map reverse $ transpose g
                    "d" -> primaryLoop  $ insertRandomTile $ map reverse $ leftGrid (map reverse g)
                    "a" -> primaryLoop  $ insertRandomTile $ leftGrid g
                    _   -> return ()

-}
{-
main :: IO ()
main = do
    let g =        [[Just 2, Just 2, Nothing, Nothing],
                    [Nothing, Nothing, Nothing, Nothing],
                    [Nothing, Nothing, Nothing, Nothing],
                    [Nothing, Nothing, Nothing, Nothing]]
    -- print $ stuckCheck g
    colorGrid g
    -- print $ randomlyPlayTilWin 0 g

    --monteCarloPlayBoard g
    primaryLoop g
    -- primaryLoop g

-}
