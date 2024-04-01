module Lib 
    (Layer(..), inDim, outDim, weights, biases, next,
     defaultLayer)
where

import Data.Matrix (Matrix)
import Data.Vector (Vector)

import qualified Data.Matrix as Mat (zero)
import qualified Data.Vector as Vec (replicate)

-- TODO: test dimensions on loading
data Layer = NilLayer | 
    Layer {
        inDim :: Integer,
        outDim :: Integer,
        weights :: Matrix Float,
        biases :: Vector Float,
        next :: Layer
    }

defaultLayer :: Layer
defaultLayer = Layer{
    inDim = 10,
    outDim = 30,
    weights = Mat.zero 10 30,
    biases = Vec.replicate 10 0,
    next = NilLayer
}

instance Show Layer where
    show NilLayer = "Nil"
    show l = show inD ++ " -> " ++ show outD
        where
            inD = inDim l
            outD = outDim l