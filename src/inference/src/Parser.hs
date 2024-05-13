-- module Parser (parseNetwork, parseCsv) where
module Parser (parseNetwork, parseCsv) where

import Numeric.LinearAlgebra.Data (Matrix, R, matrix, fromLists, tr)

import Lib(Network, Layer(Layer))

import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)

import Data.Serialize.Get (Get, runGet, getInt32le)
import Data.Serialize.IEEE754 (getFloat64le)

import Data.Vector (Vector, toList)

import Data.Csv (HasHeader(NoHeader), decode)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (unpack)

parseNetwork :: B.ByteString -> Network
parseNetwork contents = case runGet modelFileHead contents of
    Left msg -> error $ "Error parsing weights file: " ++ show msg
    Right network -> network

parseCsv :: BL.ByteString -> Matrix R
parseCsv contents = case out of
    Left msg -> error $ "Error parsing csv: " ++ show msg
    Right vec -> toMatrix vec
    where
        out :: Either String (Vector (Vector BL.ByteString))
        out = decode NoHeader contents

        toMatrix :: Vector (Vector BL.ByteString) -> Matrix R
        toMatrix v = tr $ fromLists doubles
            where
                list :: [[BL.ByteString]]
                list = tail $ toList (fmap toList v)

                doubles :: [[Double]]
                doubles = map (map (read . unpack . decodeUtf8)) list


modelFileHead :: Get Network
modelFileHead = do
    layers <- getInt32le
    inputNeurons <- getInt32le
    modelFileCont (fromIntegral layers) (fromIntegral inputNeurons)

modelFileCont :: Int -> Int -> Get Network
modelFileCont left neuronsPrev = do
    if left == 0 then return []
    else do
        neurons <- getInt32le
        weightsList <- getDoubles (fromIntegral neurons * neuronsPrev)
        biasesList <- getDoubles (fromIntegral neurons)
        let weights = matrix neuronsPrev weightsList
            biases = matrix 1 biasesList
            layer = Layer weights biases
        rest <- modelFileCont (left - 1) (fromIntegral neurons)
        return $ layer:rest
    where
        getDoubles ::  Int -> Get [Double]
        getDoubles n = do
            if n == 0 then return []
            else do
                double <- getFloat64le
                rest <- getDoubles (n - 1)
                return $ double:rest
