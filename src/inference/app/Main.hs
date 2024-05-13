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

eval :: String -> String -> Network -> IO ()
eval batchpath labpath network = do
    batchcont <- readFile batchpath
    labcont <- readFile labpath
    let matrix = parseCsv batchcont
        expected = map round (head $ toLists (parseCsv labcont)) :: [Int]
        output = mat2classes $ inferBatch network matrix
        correct = length (filter (uncurry (==)) (zip output expected))
        total = length expected
    putStrLn $ "Correctly predicted: " ++ show correct ++ "/" ++ show total
    putStrLn $ "Accuracy: " ++ show (fromIntegral correct / fromIntegral total)

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
        2 -> batch (argv !! 1) network
        3 -> eval (argv !! 1) (argv !! 2) network
        _ -> timedbatch (argv !! 1) network