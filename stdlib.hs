import Data.List
import qualified Data.Map as Map

ws = words "I  am    opfer " -- ["I", "am", "opfer"] --

xs = sort [6,2,5,8,2]

wordNums :: string -> (String, Int)
wordNums = map (\ws -> (head ws, length ws)) group . words

-- Maybe a 型 --
x = find (\x -> x^2 > 100) [1..] -- Just 11  (Just Int型)　　　--
nothing = find (<0) [1..100]  -- Nothing --

-- Map --
mapObj1 = Map.fromList[(1, "one"), (2, "two"), (3, "three")]
two = Map.lookup 2 mapObj1


