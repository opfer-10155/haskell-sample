-- Control.Applicative

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
  pure  = Just
  Nothing <*> _ = Nothing
  (Just f) (<*>) something = fmap f something

-- Just (+3) <*> Just 10 == Just 13
-- pure (+3) <*> pure 10 == Just 13
-- 多変数関数のFunctorとして使える
-- pure (+) <*> pure 3 <*> pure 10 = Just 13
-- fmap (+) Just 3 <*> Just 10 == Just 13

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

-- f <$> x <*> y
-- (+) <$> pure 3 <*> pure 10 == Just 13

instance Applicative list where
  pure x = [x]
  fs <*> xs = [fmap f x | f <- fs, x <- xs]

-- (+) <$> ["a.", "b.", "c."] <$> ["d","e","f"]
-- == ["a.d","a.e","a.f", "b.d","b.e","b.f", "c.d","c.e","c.f"]

instance Applicative IO where
  pure = return
  a <*> b = do
    f <- a
    x <- b
    return (f x)

myaction :: IO String
myaction = do
  a <- getLine
  b <- getLine
  return a ++ b

myaction' :: IO String
myaction' = (++) <$> (getLine) <$> (getLine)

instance Applicative ((->) r) where
  pure x = (\_ -> x)
  f <*> g = \x -> f x (g x)

-- tmp :: NUm a => a -> a
-- tmp = (+) <$> (+3) <*> (*100)
-- tmp = (+) . (+3) <*> (*100)
-- tmp = \x -> ((+) . (+3)) x (x*100)
-- tmp = (+(x+3)) (x*100)
-- tmp x = (x*100) + (x+3)
-- tmp 5 == 508

-- アプリカティブ則
-- pure id <*> v == v
-- pure (.) <*> u <*> v <*> w
-- == u <*> (v <*> w)
-- pure f <*> pure x == pure (f x)
-- u <*> pure y      == pure ($ y) <*> u

liftA2 :: (Applicative f) => (a -> b -> c) -> (f a) -> (f b) -> (f c)
listA2 = f a b = f <$> a <*> b

-- liftA2 (:) (Just 3) (Just [4])
-- == Just (3 : [4]) == Just [3, 4]


sequenceA :: (Applicative f) => [f a] => f [a]
sequenceA [] = pure []
sequenceA x:xs= (:) <$> x <*> sequenceA xs
-- sequenceA [(+3), (+2), (+1)]
-- (:) <$> (+3) <*> sequenceA [(+2), (+1)]
-- (:) <$> (+3) <*> ... ((:) <$> (+1) <*> \_ -> [])
-- (:) <$> (+3) <*> ... (((:) . (+1)) <*> \_ -> [])
-- (:) <$> (+3) <*> ... (\x -> ((:) . (+1)) x [])
-- (:) <$> (+3) <*> ... (\x -> (:) (x+1) [])
-- (:) <$> (+3) <*> ... (\x -> [x+1])
-- (:) <$> (+3) <*> \x -> [x+2, x+1]

