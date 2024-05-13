module Lib
    (Layer(..), Network, vec2class, mat2classes, inferSingle, inferBatch)
where

import Numeric.LinearAlgebra.Data (R, Matrix, Vector, maxIndex, toColumns, fromColumns, cmap, cols, repmat, flatten, (¿))
import Numeric.LinearAlgebra (scale, sumElements)

-- TODO: test dimensions on loading
type Network = [Layer]

data Layer = Layer {
        inDim :: Int,
        outDim :: Int,
        weights :: Matrix R,
        biases :: Matrix R
    }

-- TODO: check shape
mat2classes :: Matrix R -> [Int]
mat2classes mat = maxIndex <$> toColumns mat

vec2class :: Vector R -> Int
vec2class = maxIndex

inferBatch :: Network -> Matrix R -> Matrix R
inferBatch net input = softLayer (foldl reluLayer input nonLinear) final
    where
        size = length net
        nonLinear = take (size - 1) net
        final = last net

        nCols = cols input

        expandCols :: Matrix R -> Matrix R
        expandCols mat = repmat mat 1 nCols

        inferLayer :: Matrix R -> Layer -> Matrix R
        inferLayer inp (Layer _ _ w b) = (w <> inp) + expandCols b

        reluLayer :: Matrix R -> Layer -> Matrix R
        reluLayer inp lay = relu (inferLayer inp lay)

        softLayer :: Matrix R -> Layer -> Matrix R
        softLayer inp lay = batchSoftmax (inferLayer inp lay)

        batchSoftmax :: Matrix R -> Matrix R
        batchSoftmax mat = fromColumns (foldr folder [] [1..nCols])
            where
                folder :: Int -> [Vector R] -> [Vector R]
                folder colIdx acc =  scale (1/denom col) (cmap exp col):acc
                    where col = flatten $ mat ¿ [colIdx - 1]

                denom :: Vector R -> R
                denom col = sumElements (cmap exp col)

inferSingle :: Network -> Vector R -> Vector R
inferSingle net input = head $ toColumns $ softLayer (foldl reluLayer inpMat nonLinear) final
    where
        inpMat = fromColumns [input]
        size = length net
        nonLinear = take (size - 1) net
        final = last net

        inferLayer :: Matrix R -> Layer -> Matrix R
        inferLayer inp (Layer _ _ w b) = (w <> inp) + b

        reluLayer :: Matrix R -> Layer -> Matrix R
        reluLayer inp lay = relu (inferLayer inp lay)

        softLayer :: Matrix R -> Layer -> Matrix R
        softLayer inp lay = softmax (inferLayer inp lay)

relu :: Matrix R -> Matrix R
relu = cmap (max 0)

softmax :: Matrix R -> Matrix R
softmax v = scale (1/denom) (cmap exp v)
            where
                denom :: R
                denom = sumElements (cmap exp v)

instance Show Layer where
    show (Layer inD outD w b) = show inD ++ " -> " ++ show outD ++ "\n" ++ show w ++ "\n" ++ show b