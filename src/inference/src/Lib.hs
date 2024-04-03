module Lib
    (Layer(..), Network, inferSingle)
where

import Data.Matrix (Matrix, colVector, getCol)
import Data.Vector (Vector)

import qualified Data.Matrix as Mat (zero)
import qualified Data.Vector as Vec (replicate, map)

-- TODO: test dimensions on loading
type Network = [Layer]

data Layer = Layer {
        inDim :: Int,
        outDim :: Int,
        weights :: Matrix Double,
        biases :: Matrix Double
    }

-- TODO: check shape
inferSingle :: Network -> Vector Double -> Vector Double
inferSingle net input = getCol 1 $ softLayer (foldl reluLayer inpMat nonLinear) final
    where
            inpMat = colVector input
            size = length net
            nonLinear = take (size - 1) net
            final = last net
                        
            reluLayer inp lay = relu (inferLayer inp lay)
            softLayer inp lay = softmax (inferLayer inp lay)

inferLayer :: Matrix Double -> Layer -> Matrix Double
inferLayer input (Layer _ _ w b) = w * input + b

relu :: Matrix Double -> Matrix Double
relu = fmap (max 0)

softmax :: Matrix Double -> Matrix Double
softmax v = (/ denom) . exp <$> v
            where denom = sum (exp <$> v)


instance Show Layer where
    show (Layer inD outD w b) = show inD ++ " -> " ++ show outD ++ "\n" ++ show w ++ "\n" ++ show b