module Parser (parseNetwork, parseCsv) where

import Control.Monad(void)

import Numeric.LinearAlgebra.Data (Matrix, R, fromLists, fromList, fromColumns, tr)
import qualified Numeric.LinearAlgebra.Data as Mat (matrix)

import Lib(Network, Layer(Layer))
import Text.ParserCombinators.Parsec


parseNetwork :: String -> Network
parseNetwork contents = case runParser weightsFile () "" contents of
    Left msg -> error $ "Error parsing weights file: " ++ show msg
    Right network -> network

-- TODO: just use a regular csv parser man
parseCsv :: String -> Matrix R
parseCsv contents = case runParser csvFile () "" contents of
    Left msg -> error $ "Error parsing csv file: " ++ show msg
    Right mat -> mat

csvFile :: Parser (Matrix R)
csvFile = do
    void $ manyTill anyChar eol
    lists <- sepBy line eol
    return $ tr (fromLists lists)
    where
        line = sepBy number (char ',')
        eol = try (string "\r\n") <|> string "\n"

weightsFile :: Parser Network
weightsFile = do
    layers <- sepEndBy layer (many1 whitespace)
    eof
    return layers

layer :: Parser Layer
layer = do
    eatWhitespace
    (weights, nRows, nCols) <- matrix
    eatWhitespace
    biases <- fromList <$> vector
    return $ Layer nCols nRows weights (fromColumns [biases])

matrix :: Parser (Matrix R, Int, Int)
matrix = do
    void $ char '['
    eatWhitespace
    rows <- sepEndBy vector (many1 whitespace)
    void (char ']' <?> "matrix closing bracket")
    let nRows = length rows
    let nCols = length $ head rows
    return (Mat.matrix nCols (concat rows), nRows, nCols)

vector :: Parser [Double]
vector = do
    void $ char '['
    eatWhitespace
    numbers <- sepEndBy number (many1 whitespace)
    void (char ']' <?> "vector closing bracket")
    return numbers

number :: Parser Double
number = do
    sign <- option "" $ string "-"
    int <- many1 digit <?> "integer part"
    dec <- option "" $ do
        void $ char '.'
        frac <- many1 digit
        return $ '.' : frac
    pow <- option "" $ do
        void $ char 'e'
        esign <- option "" $ string "-" <|> string "+"
        num <- many1 digit
        let epart = 'e':(esign ++ num)
        return epart
    let num = read (sign ++ int ++ dec ++ pow) :: Double
    return num

eatWhitespace :: Parser ()
eatWhitespace = void $ many whitespace

whitespace :: Parser Char
whitespace = oneOf " \n\r\t"