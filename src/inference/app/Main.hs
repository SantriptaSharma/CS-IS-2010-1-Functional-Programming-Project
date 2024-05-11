{-# LANGUAGE BangPatterns #-}

module Main (main) where

import System.Environment(getArgs)

import Data.Matrix(toLists, transpose)
import Data.Vector(fromList)

import Control.Monad(when)

import Lib
import Parser

repl :: Network -> IO ()
repl network = do
    putStrLn "Enter sepal length, sepal width, petal length, petal width (or nothing to exit):"
    input <- getLine
    if input == "" then return ()
    else do
        let inputVec = fromList (read $ "[" ++ input ++ "]" :: [Double])
            outputVec = inferSingle network inputVec
        putStrLn $ "Class probabilities: " ++ show outputVec
        putStrLn $ "Predicted class: " ++ show (vec2class outputVec)
        repl network

batch :: String -> Network -> IO ()
batch path network = do
    contents <- readFile path
    let matrix = parseCsv contents
        output = inferBatch network matrix
    putStrLn $ "Class probabilities: " ++ show (toLists $ transpose output)
    putStrLn $ "Predicted classes: " ++ show (mat2classes output)

main :: IO ()
main = do
    argv <- getArgs
    let argc = length argv
    when (argc < 1) $ error "Usage: inference <weights file> [<batch csv file>]"
    contents <- readFile $ head argv
    let !network = parseNetwork contents
    if argc == 1 then repl network
    else batch (head (tail argv)) network