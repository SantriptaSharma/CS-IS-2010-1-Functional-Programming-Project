module Lib 
    (Layer(..), defaultLayer, Network)
where

import Data.Matrix (Matrix)
import Data.Vector (Vector)

import qualified Data.Matrix as Mat (zero)
import qualified Data.Vector as Vec (replicate)

-- TODO: test dimensions on loading
type Network = [Layer]

data Layer = Layer {
        inDim :: Int,
        outDim :: Int,
        weights :: Matrix Double,
        biases :: Vector Double
    }

-- used for testing
defaultLayer :: Layer
defaultLayer = Layer{
    inDim = 10,
    outDim = 30,
    weights = Mat.zero 10 30,
    biases = Vec.replicate 10 0
}

instance Show Layer where
    show (Layer inD outD w b) = show inD ++ " -> " ++ show outD ++ "\n" ++ show w ++ "\n" ++ show b