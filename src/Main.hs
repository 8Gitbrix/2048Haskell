{-# LANGUAGE OverloadedStrings #-}
module Main where

import HumanGame (humanPlayer)
import BotGame (botPlayer)
import Prelude

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor, attrName, simpleMain
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

drawInfo :: Widget ()
drawInfo = withBorderStyle BS.unicodeBold
  $ C.hCenter
  $ B.borderWithLabel (str "Type character of choice on the keyboard")
  $ vBox $ map (uncurry drawKey)
  $ [ ("human", "Human Player (YOU!)")
    , ("random", "Random Bot")
    , ("q", "Quit")
    ]
    where
      drawKey act key = (padRight Max $ padLeft (Pad 1) $ str act)
                        <+> (padLeft Max $ padRight (Pad 1) $ str key)

processLine :: String -> IO ()
processLine s = case s of
  "human" -> humanPlayer
  "random bot" -> botPlayer 1

main :: IO ()
main = do
  simpleMain drawInfo
  c <- getLine
  processLine c
