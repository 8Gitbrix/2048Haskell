module Game where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

start :: Int -> IO ()
start port = startGUI defaultConfig { jsPort = Just port } setup

setup :: Window -> UI ()
setup w = void $ do
    return w # set title "2048"
    UI.addStyleSheet w "2048.css"
