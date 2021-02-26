calc :: Integer -> Integer
calc n
  | n == 1 = 1
  | even n = n `div` 2
  | odd  n = 3*n + 1
  | otherwise = n

-- chain n : nから始まるコラッツ列、長さは1が出るまで --
chain :: Integer -> [Integer]
chain n
  | n == 1 = [1]
  | otherwise = n : chain m
  where m = calc n

-- 1からnまでの数のコラッツ数列の中で、長さ15以上のものの数 --
solve :: Integer -> Integer
solve n =
  let
    chains = map chain [1..n]
  in
    length filter (>=15) (map length chains)