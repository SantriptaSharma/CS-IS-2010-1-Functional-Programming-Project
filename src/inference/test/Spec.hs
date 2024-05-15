-- file Spec.hs
import Test.Hspec 
import Test.QuickCheck hiding ((><), scale)
import Control.Exception (evaluate)

import Numeric.LinearAlgebra.Data (Vector, Matrix, R, (><), matrix, fromList, fromColumns, flatten, (¿), cmap)
import Numeric.LinearAlgebra (sumElements, scale)
import System.Random (mkStdGen, genWord64)

import Data.List (unfoldr)

import GHC.Float (castWord64ToDouble)

import Lib

defaultInputVec :: Vector R
defaultInputVec = fromList [1, 2, 3, 4, 5, 6]

-- expects three-D input
defaultNetwork :: Network
defaultNetwork = [Layer ((3><3) [1..]) ((3><1) [1..]), Layer ((5><3) [1..]) ((5><1) [1..])]

w1 :: Matrix R
w1 = (5><8) [0.3, 0.5..]
b1 :: Matrix R
b1 = (5><1) [-1.32, 1.2..]

w2 :: Matrix R
w2 = (4><5) [1.2, 1.8..]
b2 :: Matrix R
b2 = (4><1) [3.2, 3.5..]

w3 :: Matrix R
w3 = (3><4) [-3.9, -3.75..]
b3 :: Matrix R
b3 = (3><1) [0.5, 0.8..]

malformedNetwork :: Network
malformedNetwork = [Layer w2 b2, Layer w3 b3, Layer w1 b1]

oneLayerNet :: Network
oneLayerNet = [Layer w1 b1, Layer w2 b2]

twoLayerNet :: Network
twoLayerNet = [Layer w1 b1, Layer w2 b2, Layer w3 b3]

randomVector :: Int -> Int -> Vector R
randomVector seed len = fromList $ toDoubles $ take len (rolls generator)
    where
        generator = mkStdGen seed

        toDoubles = map castWord64ToDouble
        rolls = unfoldr (Just . roll)
        roll = genWord64

relu :: Matrix R -> Matrix R
relu = cmap (max 0)

softmax :: Matrix R -> Matrix R
softmax v = scale (1/denom) (cmap exp v)
            where
                denom :: R
                denom = sumElements (cmap exp v)

-- TODO: fix this up by understanding quickcheck better
-- prop_single :: Network -> 

main :: IO ()
main = hspec $ do
    describe "Lib.inferSingle" $ do
        it "fails on receiving an empty network" $ do
            evaluate (inferSingle [] defaultInputVec) `shouldThrow` anyException

        it "works for correct dimensions" $ do
            inferSingle defaultNetwork (fromList [1, 2, 3]) `shouldNotBe` fromList [1, 2, 3, 4, 5]

        it "fails on receiving incorrect dimensions" $ do
            evaluate (inferSingle defaultNetwork (fromList [1, 2, 3, 4])) `shouldThrow` anyException

        it "fails on a malformed network" $ do
            evaluate (inferSingle malformedNetwork (fromList [1, 2, 3, 4, 5, 6, 7, 8])) `shouldThrow` anyException

        it "successfully acquires correct activations for one hidden layers" $
            property $ \x xs -> do
                let inp = randomVector (x :: Int) 8
                    expected = flatten $ softmax ((w2 <> relu ((w1 <> fromColumns [inp]) + b1)) + b2) ¿ [0]
                inferSingle twoLayerNet inp `shouldBe` expected

        -- it "successfully acquires correct activations for two hidden layers" $
        --     property $ \x xs -> do
        --         let inp = randomVector x 8
        --             expected = flatten $ softmax ((w3 <> relu ((w2 <> relu ((w1 <> fromColumns [inp]) + b1)) + b2)) + b3) ¿ [0]
        --         inferSingle twoLayerNet inp `shouldBe` expected