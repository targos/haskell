import Data.List
import Data.Char ( ord, chr )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Geometry.Cube as Cube

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted

decode :: Int -> String -> String
decode shift = encode (negate shift)

decoded = decode 5 . encode 5 $ "This is a sentence"

result = Cube.volume 3
