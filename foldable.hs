import qualified Data.Foldable as F

data Tree a where
  EmptyTree |
  Node a (Tree a) (Tree a)
  deriving(Eq, Show)

instance F.Foldable Tree where
  foldMap f EmptyTree = mempty
  foldMap f (Node x l r) =
    (F.foldMap f l) `mappend` (f x) `mappend` (F.foldMap f r)

instance F.Foldable [a] where
  foldMap f [] = mempty
  foldMap f x:xs = (f x) `mappend` (F.foldMap f xs)

-- foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- F.foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

-- foldr' :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- foldr' f ini xs = (F.foldMap (\x -> x) xs)
