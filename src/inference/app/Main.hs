module Main (main) where

import System.Environment(getArgs)

import Data.Vector(fromList)

import Control.Monad(when)

import Lib
import Parser

main :: IO ()

main = do
    argv <- getArgs
    let argc = length argv
    when (argc < 1) $ error "Usage: inference <weights file>"
    -- TODO: add batch inference with csv file (argc > 1)
    contents <- readFile $ head argv
    let network = parseNetwork contents
    putStrLn "Enter sepal length, sepal width, petal length, petal width:"
    input <- getLine
    let inputVec = read $ "[" ++ input ++ "]" :: [Double]
    print $ inferSingle network (fromList inputVec)