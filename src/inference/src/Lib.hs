module Lib
    (Layer(..), Network, vec2class, mat2classes, inferSingle, inferBatch)
where

import Data.List (elemIndex)
import Data.Matrix (Matrix, colVector, getCol, transpose, toLists, ncols, nrows, (<|>), fromList, submatrix)
import Data.Vector (Vector)

import qualified Data.Vector as Vec (maxIndex)

-- TODO: test dimensions on loading
type Network = [Layer]

data Layer = Layer {
        inDim :: Int,
        outDim :: Int,
        weights :: Matrix Double,
        biases :: Matrix Double
    }

-- TODO: check shape
mat2classes :: Matrix Double -> [Int]
mat2classes mat = maxInd <$> (toLists . transpose) mat
    where
        maxInd :: [Double] -> Int
        maxInd xs = case elemIndex (maximum xs) xs of
            Just i -> i
            Nothing -> error "No maximum element found?"

vec2class :: Vector Double -> Int
vec2class = Vec.maxIndex

inferBatch :: Network -> Matrix Double -> Matrix Double
inferBatch net input = softLayer (foldl reluLayer input nonLinear) final
    where
        size = length net
        nonLinear = take (size - 1) net
        final = last net

        inputCols = ncols input

        expandCols :: Matrix Double -> Matrix Double
        expandCols mat = foldr (\_ acc -> acc <|> mat) mat [1..inputCols]

        inferLayer :: Matrix Double -> Layer -> Matrix Double
        inferLayer inp (Layer _ _ w b) = w * inp + expandCols b

        reluLayer :: Matrix Double -> Layer -> Matrix Double
        reluLayer inp lay = relu (inferLayer inp lay)

        softLayer :: Matrix Double -> Layer -> Matrix Double
        softLayer inp lay = batchSoftmax (inferLayer inp lay)

        batchSoftmax :: Matrix Double -> Matrix Double
        batchSoftmax mat = submatrix 1 rows 1 inputCols (foldr folder (fromList rows 1 [1,1..]) [1..inputCols])
            where
                rows = nrows mat
                folder :: Int -> Matrix Double -> Matrix Double
                folder colIdx acc = colVector ((/ denom col) . exp <$> col) <|> acc
                    where col = getCol colIdx mat

                denom :: Vector Double -> Double
                denom col = sum (exp <$> col)

inferSingle :: Network -> Vector Double -> Vector Double
inferSingle net input = getCol 1 $ softLayer (foldl reluLayer inpMat nonLinear) final
    where
        inpMat = colVector input
        size = length net
        nonLinear = take (size - 1) net
        final = last net

        inferLayer :: Matrix Double -> Layer -> Matrix Double
        inferLayer inp (Layer _ _ w b) = w * inp + b

        reluLayer :: Matrix Double -> Layer -> Matrix Double
        reluLayer inp lay = relu (inferLayer inp lay)

        softLayer :: Matrix Double -> Layer -> Matrix Double
        softLayer inp lay = softmax (inferLayer inp lay)

relu :: Matrix Double -> Matrix Double
relu = fmap (max 0)

softmax :: Matrix Double -> Matrix Double
softmax v = (/ denom) . exp <$> v
            where denom = sum (exp <$> v)

instance Show Layer where
    show (Layer inD outD w b) = show inD ++ " -> " ++ show outD ++ "\n" ++ show w ++ "\n" ++ show b