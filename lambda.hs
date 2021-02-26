lambda = \x y -> x + y

-- flip((x, y) => z) -> (y, x) => z --
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x
