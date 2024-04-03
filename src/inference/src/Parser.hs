module Parser (parseNetwork) where

import Data.Matrix (Matrix, colVector)
import Data.Vector (Vector)

import Debug.Trace
import Control.Monad(void)

import qualified Data.Matrix as Mat (fromList)
import qualified Data.Vector as Vec (fromList)

import Lib(Network, Layer(Layer))
import Text.ParserCombinators.Parsec

parseNetwork :: String -> Network
parseNetwork contents = case runParser weightsFile () "" contents of
    Left msg -> error $ "Error parsing weights file: " ++ show msg
    Right network -> network

weightsFile :: Parser Network 
weightsFile = do
    layers <- sepEndBy layer (many1 whitespace)
    eof
    return layers

layer :: Parser Layer
layer = do
    many whitespace
    (weights, nRows, nCols) <- matrix
    many whitespace
    biases <- Vec.fromList <$> vector
    return $ Layer nCols nRows weights (colVector biases)

matrix :: Parser (Matrix Double, Int, Int)
matrix = do
    char '['
    many whitespace
    rows <- sepEndBy vector (many1 whitespace)
    char ']' <?> "matrix closing bracket"
    let nRows = length rows
    let nCols = length $ head rows
    return (Mat.fromList nRows nCols (concat rows), nRows, nCols)

vector :: Parser [Double]
vector = do
    char '['
    many whitespace
    numbers <- sepEndBy number (many1 whitespace)
    char ']' <?> "vector closing bracket"
    return numbers

number :: Parser Double
number = do
    sign <- option "" $ string "-"
    int <- many1 digit
    dec <- option "" $ do
        char '.'
        frac <- many1 digit
        return $ '.' : frac
    exp <- option "" $ do
        char 'e'
        sign <- option "" $ string "-" <|> string "+"
        num <- many1 digit
        let epart = 'e':(sign ++ num)
        return epart
    let num = read (sign ++ int ++ dec ++ exp) :: Double
    return num

whitespace = oneOf " \n\r\t"

eol =  try (string "\r\n")
    <|> string "\n"
    <?> "end of line"