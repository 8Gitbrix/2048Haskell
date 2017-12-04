{-# LANGUAGE OverloadedStrings #-}
module Game where

import Logic

import Data.Maybe
import Data.List
import Prelude

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
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
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Lens.Micro ((^.))
-- marks passing of time
data Tick = Tick

type Name = ()

-- color attributes:

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [
  (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

-- define App
app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  g <- initGame
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g

isGameOver :: Game -> Bool
isGameOver g = (checkFull (g ^. grid)) && (stuckCheck (g ^. grid))

step :: Game -> Game
step g =
  if isGameOver g then Game {_grid = g ^. grid, _score = g ^. score, _dead = True}
  else g

handle :: Direction -> Grid -> Grid
handle d g = case d of
  Logic.Up -> insertRandomTile $ transpose $ leftGrid $ transpose g
  Logic.Down -> insertRandomTile $ transpose $ map reverse $ leftGrid $ map reverse $ transpose g
  Logic.Left -> insertRandomTile $ leftGrid g
  Logic.Right -> insertRandomTile $ map reverse $ leftGrid (map reverse g)

turn :: Direction -> Game -> Game
turn dir g =
  Game {  _grid = newGrid
        , _score = (scoreGrid newGrid 0)
        , _dead = (checkFull newGrid && stuckCheck newGrid)
        }
  where newGrid = handle dir (g ^. grid)

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ turn Logic.Up g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ turn Logic.Down g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ turn Logic.Left g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ turn Logic.Right g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

-- Drawing
drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (g ^. score)
         , padTop (Pad 2) $ drawGameOver (g ^. dead)
         ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
    then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
    else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "2048")
  $ vBox rows
  where
    rows = [hBox $ tilesInRow r | r <- (g ^. grid)]
    tilesInRow row = [str $ printTile tile | tile <- row]
