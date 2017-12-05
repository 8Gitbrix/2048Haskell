{-# LANGUAGE TemplateHaskell #-}

module Logic (Game(..), Direction(..), Grid, printTile, initGame,
              insertRandomTile, stuckCheck, leftGrid, checkFull, scoreGrid,
              mainLogic)
        where

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

-- findBestMove :: Grid -> Int
-- findBestMove g = do
--    let up = insertRandomTile $ transpose $ leftGrid $ transpose g
--    let down = insertRandomTile $ transpose $ map reverse $ leftGrid $ map reverse $ transpose g
--    let right = insertRandomTile $ map reverse $ leftGrid (map reverse g)
--    let left = insertRandomTile $ leftGrid g

--    let upScore = randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up+ randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up + randomlyPlayBoard up
--    let downScore =  randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down  + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down  + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down  + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down + randomlyPlayBoard down
--    let rightScore = randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right + randomlyPlayBoard right
--    let leftScore =  randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left  + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left  + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left  + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left + randomlyPlayBoard left

--    let maxScore = maximum [upScore,downScore,rightScore,leftScore]
--    if maxScore == upScore then 1
--    else if maxScore == downScore then 2
--        else if maxScore == rightScore then 3
--             else if maxScore == leftScore then 4
--             else 4

-- average :: [Int] -> Int
-- average xs = (sum xs) `div` (length xs)

-- oneBestMove :: Grid -> Grid
-- oneBestMove g = do
--     case (findBestMove g) of
--        1 -> insertRandomTile $ transpose $ leftGrid $ transpose g
--        2 -> insertRandomTile $ transpose $ map reverse $ leftGrid $ map reverse $ transpose g
--        3 -> insertRandomTile $ map reverse $ leftGrid (map reverse g)
--        4 -> insertRandomTile $ leftGrid g

-- monteCarloPlayBoard :: Grid -> IO ()
-- monteCarloPlayBoard g = do
--    if checkFull g && stuckCheck g then colorGrid g
--    else monteCarloPlayBoard $ oneBestMove g

-- randomlyPlayBoard :: Grid -> Int
-- randomlyPlayBoard g = do
--    if checkFull g && stuckCheck g then scoreGrid g 0
--    else randomlyPlayBoard $ oneMove g

-- randomlyPlayTilWin :: Int -> Grid -> Int
-- randomlyPlayTilWin n g = do
--    if randomlyPlayBoard g == 2048 then n
--    else if n == 10000 then n
--         else randomlyPlayTilWin (n+1) g

-- oneMove :: Grid -> Grid
-- oneMove g = do
--     let (randomDirection) = unsafePerformIO $ getStdRandom $ randomR (1,4)
--     if (randomDirection::Int) == 1 then insertRandomTile $ transpose $ leftGrid $ transpose g
--     else if (randomDirection::Int) == 2 then insertRandomTile $ transpose $ map reverse $ leftGrid $ map reverse $ transpose g
--          else if (randomDirection::Int) == 3 then insertRandomTile $ map reverse $ leftGrid (map reverse g)
--               else if (randomDirection::Int) == 4 then insertRandomTile $ leftGrid g
--                    else g
directionStuckHeuristic :: Int -> Grid -> Int
directionStuckHeuristic n g = case n of
    1 -> if g == (transpose $ leftGrid $ transpose g) then (-999999) else 0
    2 -> if g == (transpose $ map reverse $ leftGrid $ map reverse $ transpose g) then (-999999) else 0
    3 -> if g == (map reverse $ leftGrid (map reverse g)) then (-999999) else 0
    4 -> if g == (leftGrid g) then (-999999) else 0

directionStuckCheck :: Int -> Grid -> Bool
directionStuckCheck n g = case n of
    1 -> if g == (transpose $ leftGrid $ transpose g) then True else False
    2 -> if g == (transpose $ map reverse $ leftGrid $ map reverse $ transpose g) then True else False
    3 -> if g == (map reverse $ leftGrid (map reverse g)) then True else False
    4 -> if g == (leftGrid g) then True else False

optimizeWeight :: Grid -> Int -> Int -> Int -> Int -> Int
optimizeWeight g x y z i = do
  -- let g =           [[Just 2, Just 2, Nothing, Nothing],
  --                   [Nothing, Nothing, Nothing, Nothing],
  --                   [Nothing, Nothing, Nothing, Nothing],
  --                   [Nothing, Nothing, Nothing, Nothing]]
  let tempScore = findAverage5Score g x y z
  case i of
    1 -> if (findAverage5Score g (x+1) y z) > (tempScore) then optimizeWeight g (x+1) y z 1
         else if (findAverage5Score g (x-1) y z) > (tempScore) then optimizeWeight g (x-1) y z 1
              else x
    2 -> if (findAverage5Score g x (y+1) z) > (tempScore) then optimizeWeight g x (y+1) z 2
         else if (findAverage5Score g x (y-1) z) > (tempScore) then optimizeWeight g x (y-1) z 2
              else y
    3 -> if (findAverage5Score g x y (z+1)) > (tempScore) then optimizeWeight g x y (z+1) 3
         else if (findAverage5Score g x y (z-1)) > (tempScore) then optimizeWeight g x y (z-1) 3
              else z

determineOptimalWeights :: Grid -> Int -> Int -> Int -> (Int, Int, Int)
determineOptimalWeights g x y z =
  -- let g =        [[Just 2, Just 2, Nothing, Nothing],
  --                   [Nothing, Nothing, Nothing, Nothing],
  --                   [Nothing, Nothing, Nothing, Nothing],
  --                   [Nothing, Nothing, Nothing, Nothing]]
  ((optimizeWeight g x y z 1),(optimizeWeight g x y z 2), (optimizeWeight g x y z 3))

findBestMove :: Grid -> (Int, Int, Int) ->  Int
findBestMove g (x,y,z)= do
   let up = insertRandomTile $ transpose $ leftGrid $ transpose g
   let down = insertRandomTile $ transpose $ map reverse $ leftGrid $ map reverse $ transpose g
   let right = insertRandomTile $ map reverse $ leftGrid (map reverse g)
   let left = insertRandomTile $ leftGrid g

    -- if (up == g) then
      -- let upScore = -9999
   -- else
   let upScore =(directionStuckHeuristic 1 g) + (((largeEdgeNumberHeuristic up)*z) + (((monotonicityHeuristic up) + (monotonicityHeuristic $ transpose up))*y) +(x* ((mergesHeuristic up) + (mergesHeuristic $ transpose up) + (openSquareHeuristic up))))
   -- if down == g then
   --    let downScore = -9999
   -- else
   let downScore = (directionStuckHeuristic 2 g) + (((largeEdgeNumberHeuristic down)*z)  + (((monotonicityHeuristic down) + (monotonicityHeuristic $ transpose down))*y) +(x* ((mergesHeuristic down) + (mergesHeuristic $ transpose down) + (openSquareHeuristic down))))
   -- if right == g then
   -- 	  let rightScore = -9999
   -- else
   let rightScore =(directionStuckHeuristic 3 g) + (((largeEdgeNumberHeuristic right)*z) + (((monotonicityHeuristic right) + (monotonicityHeuristic $ transpose right))*y) +(x* ((mergesHeuristic right) + (mergesHeuristic $ transpose right) + (openSquareHeuristic right))))
   -- if left == g then
   --    let leftScore = -9999
   -- else
   let leftScore =(directionStuckHeuristic 4 g) + (((largeEdgeNumberHeuristic left)*z)+ (((monotonicityHeuristic left) + (monotonicityHeuristic $ transpose left))*y) + (x*((mergesHeuristic left) + (mergesHeuristic $ transpose left) + (openSquareHeuristic left))))


   -- let upScore = runRandomN up 50
   -- let downScore = runRandomN down 50
   -- let rightScore = runRandomN right 50
   -- let leftScore = runRandomN left 50

   let maxScore = maximum [upScore,downScore,rightScore,leftScore]
   if maxScore == upScore then 1
   else if maxScore == downScore then 2
       else if maxScore == rightScore then 3
            else if maxScore == leftScore then 4
            else 4

runRandomN :: Grid -> Int -> Int
runRandomN g n = do
    if n == 0 then randomlyPlayBoard g
    else randomlyPlayBoard g + runRandomN g (n-1)

largeEdgeNumberHeuristic :: Grid -> Int
largeEdgeNumberHeuristic g = do
    if (a /= Nothing && tempScore == (fromJust a)) || (b /= Nothing && tempScore == (fromJust b)) || (c /= Nothing && tempScore == (fromJust c)) || (d /= Nothing && tempScore == (fromJust d)) || (e /= Nothing && tempScore == (fromJust e)) || (f /= Nothing && tempScore == (fromJust f)) || (gg /= Nothing && tempScore == (fromJust gg)) || (h /= Nothing && tempScore == (fromJust h)) || (i /= Nothing && tempScore == (fromJust i)) || (j /= Nothing && tempScore == (fromJust j)) || (k /= Nothing && tempScore == (fromJust k)) || (l /= Nothing && tempScore == (fromJust l)) then (myExp tempScore) else 0
       where
         [[a,b,c,d],[e,_,_,f],[gg,_,_,h],[i,j,k,l]] = g
         tempScore = scoreGrid g 0



-- randomlyPlayBoard g = do
--    if checkFull g && stuckCheck g then scoreGrid g 0
--    else randomlyPlayBoard $ oneMove g

-- generateRandomBoard :: Grid
-- generateRandomBoard = do
--     let a = generateRandomRow
--     let b = generateRandomRow
--     let c = generateRandomRow
--     let d = generateRandomRow
--     [a,b,c,d]

-- generateRandomRow :: [Tile]
-- generateRandomRow = do
--     let a = generateRandomTile
--     let b = generateRandomTile
--     let c = generateRandomTile
--     let d = generateRandomTile
--     [a,b,c,d]

-- generateRandomTile :: Tile
-- generateRandomTile = do
--     let (tempNum) = unsafePerformIO $ getStdRandom $ randomR (1,6)
--     if (tempNum::Int) == 1 then Just 2
--     else if (tempNum::Int) == 2 then Just 4
--         else if (tempNum::Int) == 3 then Just 8
--              else if (tempNum::Int) == 4 then Just 16
--                    else if (tempNum::Int) == 5 then Just 32
--                         else if (tempNum::Int) == 6 then Just 64
--                             else Just 2



mergesHeuristic :: Grid -> Int
mergesHeuristic g = case g of
    [] -> 0
    (x:xs) -> (countMergesInRow $ removeNothings x) + (mergesHeuristic xs)

-- rowMerges :: [Tile] -> Int
-- rowMerges g = case g of
--     [a,b,c,d] ->

countMergesInRow :: [Tile] -> Int
countMergesInRow r = case r of
    [] -> 0
    (x:[]) -> 0
    (x:xs) -> if x == (head xs) then 1 + (countMergesInRow xs)
              else (countMergesInRow xs)

removeNothings :: [Tile] -> [Tile]
removeNothings r = case r of
    [] -> []
    (x:xs) -> do
        case x of
            Nothing -> removeNothings xs
            (Just a) -> (Just a):(removeNothings xs)

monotonicityHeuristic :: Grid -> Int
monotonicityHeuristic g = case g of
    [] -> 0
    (x:xs) -> (checkRowMonotonicity $ removeNothings x) + (monotonicityHeuristic xs)

checkRowMonotonicity :: [Tile] -> Int
checkRowMonotonicity r = do
    if (checkRowIncreasing r) || (checkRowDecreasing r) then 1 else 0

checkRowIncreasing :: [Tile] -> Bool
checkRowIncreasing r = case r of
    [] -> True
    (x:[]) -> True
    (x:xs) -> if x <= (head xs) then True && (checkRowIncreasing xs)
              else False

checkRowDecreasing :: [Tile] -> Bool
checkRowDecreasing r = case r of
    [] -> True
    (x:[]) -> True
    (x:xs) -> if x >= (head xs) then True && (checkRowDecreasing xs)
              else False

openSquareHeuristic :: Grid -> Int
openSquareHeuristic g = case g of
    [] -> 0
    (x:xs) -> countNothingInRow x + openSquareHeuristic xs

countNothingInRow :: [Tile] -> Int
countNothingInRow r = case r of
    [] -> 0
    (Nothing:xs) -> 1 + countNothingInRow xs
    (_:xs) -> countNothingInRow xs


-- average :: [Int] -> Int
-- average xs = (sum xs) `div` (length xs)

oneBestMove :: Grid -> (Int, Int, Int) ->  Grid
oneBestMove g (x,y,z) = do
    case (findBestMove g (x,y,z)) of
       1 -> insertRandomTile $ transpose $ leftGrid $ transpose g
       2 -> insertRandomTile $ transpose $ map reverse $ leftGrid $ map reverse $ transpose g
       3 -> insertRandomTile $ map reverse $ leftGrid (map reverse g)
       4 -> insertRandomTile $ leftGrid g

findAverage5Score :: Grid -> Int -> Int -> Int -> Int
findAverage5Score g x y z = do
   let a = monteCarloPlayBoard g 0 (x,y,z)
   let b = monteCarloPlayBoard g 0 (x,y,z)
   let c = monteCarloPlayBoard g 0 (x,y,z)
   let d = monteCarloPlayBoard g 0 (x,y,z)
   let e = monteCarloPlayBoard g 0 (x,y,z)
   let f = monteCarloPlayBoard g 0 (x,y,z)
   let gg = monteCarloPlayBoard g 0 (x,y,z)
   let h = monteCarloPlayBoard g 0 (x,y,z)
   let i = monteCarloPlayBoard g 0 (x,y,z)
   let j = monteCarloPlayBoard g 0 (x,y,z)
   ((myExp a) + (myExp b) + (myExp c) + (myExp d) + (myExp e) + (myExp f) + (myExp gg) + (myExp h) + (myExp i) + (myExp j))

myExp :: Int -> Int
myExp a =
  let x = fromIntegral a
  in (floor ( logBase 2 x ))

myDiv :: Int -> Int -> Int
myDiv a b =
      let x = fromIntegral a
          y = fromIntegral b
      in quot a b

monteCarloPlayBoard :: Grid -> Int -> (Int, Int, Int) -> Int
monteCarloPlayBoard g n (x,y,z) = do
   if (checkFull g && stuckCheck g) then scoreGrid g 0
   else monteCarloPlayBoard (oneBestMove g (x,y,z)) (n+1) (x,y,z)

randomlyPlayBoard :: Grid -> Int
randomlyPlayBoard g = do
   if checkFull g && stuckCheck g then scoreGrid g 0
   else randomlyPlayBoard $ oneMove g
-- randomlyPlayBoard :: Grid -> IO ()
-- randomlyPlayBoard g = do
--     if checkFull g && stuckCheck g then colorGrid g
--     else randomlyPlayBoard $ oneMove g

randomlyPlayTilWin :: Int -> Grid -> Int
randomlyPlayTilWin n g = do
   if randomlyPlayBoard g == 2048 then n
   else if n == 10000 then n
        else randomlyPlayTilWin (n+1) g

        -- let randomDirection = unsafePerformIO $ getStdRandom $ random (1,4)
        -- if (randomDirection::Int) == 1 then do
        -- insertRandomTile $ transpose $ leftGrid $ transpose g

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
  , _done  :: Bool
  } deriving (Eq, Show)

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Eq, Show)

-- add options for bot later
initGame :: IO Game
initGame = do
  pure $
    Game { _grid = [[Just 2, Just 2, Nothing, Nothing],
                    [Nothing, Nothing, Nothing, Nothing],
                    [Nothing, Nothing, Nothing, Nothing],
                    [Nothing, Nothing, Nothing, Nothing]]
        , _score = 0
        , _done = False
        }


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
                    "w" -> if (directionStuckCheck 1 g) then primaryLoop g else primaryLoop $ insertRandomTile $ transpose $ leftGrid $ transpose g
                    "s" -> if (directionStuckCheck 2 g) then primaryLoop g else primaryLoop  $ insertRandomTile $ transpose $ map reverse $ leftGrid $ map reverse $ transpose g
                    "d" -> if (directionStuckCheck 3 g) then primaryLoop g else primaryLoop  $ insertRandomTile $ map reverse $ leftGrid (map reverse g)
                    "a" -> if (directionStuckCheck 4 g) then primaryLoop g else primaryLoop  $ insertRandomTile $ leftGrid g
                    _   -> return ()


mainLogic ::IO ()
mainLogic = do
    let g =        [[Just 2, Just 2, Nothing, Nothing],
                    [Nothing, Nothing, Nothing, Nothing],
                    [Nothing, Nothing, Nothing, Nothing],
                    [Nothing, Nothing, Nothing, Nothing]]
    -- let (x,y,z) = determineOptimalWeights g 4 1 8
    -- print (x,y,z)
    let b = monteCarloPlayBoard g 0 (5,1,8)
    print b

    --primaryLoop g
    -- primaryLoop g
