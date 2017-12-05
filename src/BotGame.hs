{-# LANGUAGE OverloadedStrings #-}
module BotGame (botPlayer2) where

import Logic (Game(..), Direction(..), Grid, keepTrying)
import HumanGame (Name, drawUI, theMap, initGame, move, Tick)
import Data.Maybe
import Data.List
import Prelude

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor, attrName
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))

-- define App
app :: App Game String Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = botEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

--processAction :: Int -> Grid
--processAction i = case i of
  --1 ->

botPlayer2 :: [String] -> IO ()
botPlayer2 s = do
  chan <- newBChan 10
  forkIO $ forever $ do
    --processAction i
    mapM (writeBChan chan) ["Up", "Down"]
    -- writeBChan chan "Up"
    threadDelay 100000 -- decides how fast your game moves
  g <- initGame
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g


botEvent :: Game -> BrickEvent Name String -> EventM Name (Next Game)
botEvent g (AppEvent s) = case s of
  "Up" -> continue $ move Logic.Up g
  "Down" -> continue $ move Logic.Down g
  "Right" -> continue $ move Logic.Right g
  "Left" -> continue $ move Logic.Left g


-- botPlayer :: Int -> IO ()
-- botPlayer i = do
--   chan <- newBChan 10
--   forkIO $ forever $ do
--     --processAction i
--     writeBChan chan "Up"
--     threadDelay 100000 -- decides how fast your game moves
--   g <- initGame
--   void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g

-- botEvent :: Game -> BrickEvent Name String -> EventM Name (Next Game)
-- botEvent g (AppEvent s) = continue $ move Logic.Up g
