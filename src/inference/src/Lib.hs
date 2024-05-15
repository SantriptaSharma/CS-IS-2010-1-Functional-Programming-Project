module Lib
    (Layer(..), Network, vec2class, mat2classes, inferSingle, inferBatch, softmax, relu)
where

import Numeric.LinearAlgebra.Data (R, Matrix, Vector, maxIndex, toColumns, fromColumns, cmap, rows, cols, repmat, flatten, (¿), maxElement)
import Numeric.LinearAlgebra (scale, sumElements)

type Network = [Layer]

data Layer = Layer {
        weights :: Matrix R,
        biases :: Matrix R
    }

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
        inferLayer inp (Layer w b) = (w <> inp) + expandCols b

        reluLayer :: Matrix R -> Layer -> Matrix R
        reluLayer inp lay = relu (inferLayer inp lay)

        softLayer :: Matrix R -> Layer -> Matrix R
        softLayer inp lay = batchSoftmax (inferLayer inp lay)

        batchSoftmax :: Matrix R -> Matrix R
        batchSoftmax mat = fromColumns (foldr folder [] [1..nCols])
            where
                folder :: Int -> [Vector R] -> [Vector R]
                folder colIdx acc =  scale (1/denom) expCol:acc
                    where
                        col = flatten $ mat ¿ [colIdx - 1]

                        colMax = maxElement col
                        correctedCol = cmap (\x -> x - colMax) col

                        expCol = cmap exp correctedCol
                        denom = sumElements expCol

inferSingle :: Network -> Vector R -> Vector R
inferSingle net input = head $ toColumns $ softLayer (foldl reluLayer inpMat nonLinear) final
    where
        inpMat = fromColumns [input]
        size = length net
        nonLinear = take (size - 1) net
        final = last net

        inferLayer :: Matrix R -> Layer -> Matrix R
        inferLayer inp (Layer w b) = (w <> inp) + b

        reluLayer :: Matrix R -> Layer -> Matrix R
        reluLayer inp lay = relu (inferLayer inp lay)

        softLayer :: Matrix R -> Layer -> Matrix R
        softLayer inp lay = softmax (inferLayer inp lay)

relu :: Matrix R -> Matrix R
relu = cmap (max 0)

softmax :: Matrix R -> Matrix R
softmax v = scale (1/denom) expV
            where
                -- shift max elem to 0, for additional numerical stability during exponentiation
                -- (prevents large numbers from NaN'ing, common implementation detail for softmax) 
                highest :: R
                highest = maxElement v

                correctedV :: Matrix R
                correctedV = cmap (\x -> x - highest) v

                expV :: Matrix R
                expV = cmap exp correctedV

                denom :: R
                denom = sumElements expV

instance Show Layer where
    show (Layer w b) = show (cols w) ++ " -> " ++ show (rows w) ++ "\n" ++ show w ++ "\n" ++ show b