-- モノイド
-- 1. 閉じた二項演算 _~_
-- 2. 結合法則 (x ~ y) ~ z == x ~ (y ~ z)
-- 3. 単位元の存在 x ~ e == e ~ x == x


class Monoid' m where
  mempty :: m            -- 単位元
  mappend :: m -> m -> m -- 二項演算
  mconcat :: [m] -> m    -- listを二項演算で畳み込み
  mconcat = foldr mappend mempty

-- MonoidクラスにMonoidの性質を満たしているかの検査機能はない

-- (list, ++)はモノイド
instance Monoid [a] where
  mempty = []
  mappend = (++)


-- 数値の和と積のモノイドを同時に定義するには

newtype Sum' a = Sum' { getSum' :: a }
  deriving(Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid' (Sum' a) where
  mempty = Sum' 1
  (Sum' x) `mappend` (Sum' y) = Sum'(x + y)


newtype Product' a = Product' { getProduct :: a }
  deriving(Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid' (Product' a) where
  mempty = Product' 1
  (Product' x) `mappend` (Product' y) = Product'(x * y)


