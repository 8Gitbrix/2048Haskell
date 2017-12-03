module Logic where

import Data.Maybe
import Data.List
import Prelude

import System.Random
import Control.Monad.IO.Class
import System.IO.Unsafe


-- import System.IO (stdin, hReady)
-- import System.IO (stdin, hSetEcho)
-- import Control.Monad (when)
-- import System.Exit


-- import Control.Monad.IO.Class
-- import System.Console.Haskeline
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
colorRow (x:xs) = do putStr $ (printTile x) ++ "   "
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
  (x:xs) -> checkRowExists x && checkFull xs

checkRowExists :: [Tile] -> Bool
checkRowExists r = case r of
  [] -> True
  (x:xs) -> checkTileExists x && checkRowExists xs

checkTileExists :: Tile -> Bool
checkTileExists t = case t of
  Nothing -> False
  _ -> True

insertRandomTile :: Grid -> Grid
insertRandomTile  g = case g of  
   (x:xs) -> if checkRowExists x then x:(insertRandomTile xs) 
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
               if checkRowExists xs then (makeRandomTile):xs
               else do
                   let (tempNum) = unsafePerformIO $ getStdRandom $ randomR (1,10)
                   if (tempNum::Int) <= 6 then x:(insertRandomTileInRow xs)
                    else (makeRandomTile):xs

-- insertRandomTile :: Grid -> Grid
-- insertRandomTile g = case g of 
--    (x:xs) -> if checkRowExists x then x:(insertRandomTile xs) 
--              else (insertRandomTileInRow  x):xs

-- insertRandomTileInRow :: [Tile] -> [Tile]
-- insertRandomTileInRow r = case r of
--    (x:xs) -> if checkTileExists x then x:(insertRandomTileInRow xs)
--              else (makeRandomTile):xs
              
            
makeRandomTile :: Tile
makeRandomTile = do
   let (tempNum) = unsafePerformIO $ getStdRandom $ randomR (1,10)
   if (tempNum::Int) < 10 then Just 2 else Just 4
       
-- makeRandomTile = do
--    let (num, rand') = R.randomR (1,10) rand
--    if (num::Int) <= 9 then Just 2 else Just 4
-- changeD :: IO Int -> Tile
-- changeD 


   
-- roll :: (StdGen -> (a, StdGen)) -> IO a -> Int
-- roll stdg = do unsafePerformIO $ stdg $ randomR (1,10)

primaryLoop :: Grid -> IO ()
primaryLoop g = do
    
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

main = do

    let g =        [[Just 2, Just 2, Nothing, Nothing],
                    [Nothing, Nothing, Nothing, Nothing],
                    [Nothing, Nothing, Nothing, Nothing],
                    [Nothing, Nothing, Nothing, Nothing]]
    -- print $ stuckCheck g
    
    primaryLoop g
    -- primaryLoop g
 
    





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
