head' :: (Show a) => [a] -> String
head' [] = "empty"
head' all@(x:xs) = "The head of " ++ show all ++ " is" ++ show x

guard x
  | x > 100 = "more than 100"
  | x < 100 = "less than 100"
  | otherwise = "equal to 100"

bmi w h
  | bmi <= skinny = "yase"
  | bmi <= normal = "normal"
  | bmi <= fat = "debu"
  where
    bmi = w / h^2
    skinny = 18.5
    normal = 25.0
    fat = 30.0

tail' :: [a] -> [a]
tail' xs = case xs of
  [] -> error "empty"
  (_:xs) -> xs

-- take' 3 [1,2,3,4,5] -> [1,2,3]
take' :: Int -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs


qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (pivot:xs) =
  let
    lessOrEq = [x | x <- xs, x <= pivot]
    more = [x | x <- xs, x > pivot]
  in
    qsort lessOrEq ++ [pivot] ++ more

qsort' :: (Ord a) => [a] -> [a]
qsort' [] = []
qsort' (pivot:xs) =
  let
    lessOrEq = filter (<= pivot) xs
    more = filter (> pivot) xs
  in
    qsort lessOrEq ++ [pivot] ++ more


-- flip((x, y) => z) -> (y, x) => z --
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f  = g
  where g y x = f x y

-- flip'と同じ --
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

map' f xs = foldr (\x acc -> (f x) : acc) [] xs

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

filter' f = foldl (\acc x -> if f x then x:acc else acc ) []

scanl' f acc xs = foldl (\acc' x -> acc' ++ [f (last acc') x]) [acc] xs

scanr' f acc xs = foldr (\x acc' -> (f x (head acc')) : acc') [acc] xs

-- 条件に合う要素を列挙 --
-- 条件に合わない要素が出てくると停止する, 無限リストの走査に使える --
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f xs = case xs of
  [] -> []
  x:xs -> (if f x then x:rest else rest)
  where
    rest = takeWhile' f xs

-- 括弧の削減や関数適応自体を関数としてあつかうときに --
-- f $ x == (f) (x) --
($') :: (a -> b) -> a -> b
f $' x = f x

-- 関数合成 --
-- (f . g) x == f (g x) --
(.') :: (b -> c) -> (a -> b) -> a -> c
f .' g = \x -> f (g x)


fn x = tan (cos (max 50 x))
-- fn = cos (max 50) は type error だが --
fn' = tan . cos . (max 50)
