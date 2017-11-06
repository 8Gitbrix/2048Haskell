module Main where

import System.Environment (getArgs)
import System.IO
import Game               (start)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [port, otherArg1, otherArg2] <- getArgs
    start (read port)
