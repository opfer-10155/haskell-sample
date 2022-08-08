module Zipper where

-- コモナドの具体例
-- Zipperの一般化がコモナド？

import Comonad

-- 1つのリストの中のある要素に注目しているような形
data Zipper a =
  Z [a] a [a]

left, right :: Zipper a -> Zipper a
left (Z (l:ls) c rs) = Z ls l (c:rs)
-- left z = z

right (Z ls c (r:rs)) = Z (c:ls) r rs
-- right z = z

-- iterateの適用が1回以上の部分
iterate1 :: (a -> a) -> a -> [a]
iterate1 f = tail . iterate f

instance Functor Zipper where
  fmap f (Z ls c rs) = Z (fmap f ls) (f c) (fmap f rs)

instance Comonad Zipper where
  -- 注目している値を取り出す
  extract (Z _ a _) = a

  -- 注目する場所を左右にずらした全てのZipperを集めたZipper
  duplicate z = Z (iterate1 left z) z (iterate1 right z)

