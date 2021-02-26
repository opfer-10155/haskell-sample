-- (写像が定義できるもの)の型クラス

-- fは抽象型だ！
class Functer' f where
  fmap :: (a -> b) -> f a -> f b

-- map関数はFuncter list
instance Functer' [] where
  fmap = map

-- Maybe
instance Functer' Maybe where
  fmap f (just x) = just (f x)
  fmap _ Nothing  = Nothing

-- 同じやつ
(>>=) :: Maybe a -> (a -> b) -> Maybe b
(just x) >>= f = f x
Nothing  >>= f = Nothing

-- :info Functor
-- class Functor (f :: * -> *) where
--   fmap :: (a -> b) -> f a -> f b
--   (<$) :: a -> f b -> f a
--   {-# MINIMAL fmap #-}
--         -- Defined in ‘GHC.Base’
-- instance Functor (Either a) -- Defined in ‘Data.Either’
-- instance Functor [] -- Defined in ‘GHC.Base’
-- instance Functor Maybe -- Defined in ‘GHC.Base’
-- instance Functor IO -- Defined in ‘GHC.Base’
-- instance Functor ((->) r) -- Defined in ‘GHC.Base’
-- instance Functor ((,) a) -- Defined in ‘GHC.Base’


-- IOのmapはIOアクションを実行し結果に関数を適用させる
-- instance Functor IO
-- fmap f action = do
--   a <- action
--   return (f a)

-- 関数型のmapは関数合成
-- instance Functor ((->) r)
--   fmap = (.)

-- fmap :: (a -> b) -> ((->) r a) -> ((->) r b)

-- fmap (*2) :: (Functor f, Num b) => f b -> f b

-- fmapの考え方
-- fmapは関数とFunctorを受け取って、その関数でFunctor値を写して返す
-- fmapは値から値への関数を受け取って、それをFunctor値からFunctor値への関数にして返す

-- Functor則(設計思想)
-- 第一法則: 任意のFunctorはfmap id = idとならなくてはならない
-- 第二法則: fmap (f . g) = fmap f . fmap g
-- 例)
-- fmap (f . g) Nothing = Nothing
-- fmap f (fmap g Nothing) = Nothing
-- fmap (f . g) Just x = Just ((f . g) x) = Just (f (g x))
-- (fmap f . fmap g) Just x = fmap f (fmap g (Just x)) = fmap f (Just (g x)) = Just (f (g x))

