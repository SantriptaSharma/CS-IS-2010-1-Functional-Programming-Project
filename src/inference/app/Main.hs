{-# LANGUAGE BangPatterns #-}

module Main (main) where

import System.Environment(getArgs)
import System.TimeIt

import Numeric.LinearAlgebra.Data(toLists, tr, fromList)
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
    putStrLn $ "Class probabilities: " ++ show (toLists $ tr output)
    putStrLn $ "Predicted classes: " ++ show (mat2classes output)

timedbatch :: String -> Network -> IO ()
timedbatch path network = do
    contents <- readFile path
    let matrix = parseCsv contents
    timeIt $ infer network matrix
    where 
        infer n m = do
            let !_ = inferBatch n m
            return ()

main :: IO ()
main = do
    argv <- getArgs
    let argc = length argv
    when (argc < 1) $ error "Usage: inference <weights file> [<batch csv file>]"
    contents <- readFile $ head argv
    let !network = parseNetwork contents
    case argc of
        1 -> repl network
        2 -> batch (head (tail argv)) network
        3 -> batch (head (tail argv)) network
        _ -> timedbatch (head (tail argv)) network