-- importは必ず式を書く前に
import Data.List
import Data.List (nub, sort) -- importするものを限定 --
import Data.List hiding (nub) -- nubを除く --

-- 自分のモジュール ./Mod以下 --
-- フォルダ名・ファイル名とモジュール名を一致させておく --
import qualified Mod.Parent
import qualified Mod.Sub

import qualified Data.Map (map)
-- Data.Mapのmap関数にData.Map.mapでアクセス可能 --
-- qualifiedなくてもできるけど… --
-- `map`関数を汚染しない --

import qualified Data.Map as M (filter)
-- Data.Mapのfilter関数にM.filterでアクセス可能 --


-- nub: Data.Listモジュールの関数、listをユニークにする --
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
