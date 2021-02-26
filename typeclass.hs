-- 型クラス宣言
-- メソッドの型定義を与える
class Eq' a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x == y = not (x /= y) -- 最小完全定義
  x /= y = not (x == y) -- 最小完全定義 片方の実装を与えるだけでよくなる

data Color = Red | Blue | Yellow
data Maybe' a = Just' a | Nothing'

-- instance
-- 型クラスメソッドの実装を与える
-- (class宣言の型引数にColorを与えているね)
instance Eq' Color where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

-- 自動導出の原理
instance (Eq' m) => Maybe m where
  Just' x  == Just' y  = x == y
  Nothing' == Nothing' = True
  _ == _               = False

-- サブクラス
class (Eq a) => Num' a where
  (+) : a -> a -> a

