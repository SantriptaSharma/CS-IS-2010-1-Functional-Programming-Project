module Parser
    (parseNetwork)
where
   
import Lib (Layer(..), defaultLayer)
import Data.Char (isSpace)

feats :: Integer
classes :: Integer

feats = 4
classes = 3

-- TODO: test invalid files
parseNetwork :: [Char] -> Maybe Layer
parseLayer :: [Char] -> (Layer, [Char])

parseNetwork [] = Nothing
parseNetwork contents | all isSpace contents = Nothing
parseNetwork contents = Just defaultLayer

parseLayer c = (NilLayer, c)

readWeightsFile :: [Char] -> [Char]
readWeightsFile c = c