module Main (main) where

import System.Environment(getArgs)

import Lib
import Parser

main :: IO ()

main = do
    args <- getArgs
    contents <- readFile $ head args
    let network = parseNetwork contents
    mapM_ print network