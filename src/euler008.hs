import System.IO
import Data.Char

-- Largest product in a series
-- TODO(Bryan): This one is postponed until I get to the Monads chapter of LYAH...
euler6 :: Int -> Int
euler6 x = readFile file >>= largestProduct x
    where file = "euler008_resource.txt"

largestProduct :: Int -> String -> Int
largestProduct x contents = maximum (map f [0..(length contents) - x])
    where f = getRunningProduct x contents

getRunningProduct :: Int -> String -> Int -> Int
getRunningProduct x contents start = product (map ord (take x (drop start contents)))
