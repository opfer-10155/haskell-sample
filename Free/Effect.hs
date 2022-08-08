{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

data Term f a =
  Pure a |
  Impure (f (Term f a))

instance Functor f => Functor (Term f) where
  fmap f (Pure x)   = Pure $ f x
  fmap f (Impure c) = Impure $ fmap (fmap f) c

-- ファンクタfをモナドにする高階関手
instance Functor f => Monad (Term f) where
  return = Pure
  Pure x >>= f = f x
  Impure c >>= f = Impure $ fmap (>>= f) c

data Zero a
data One a = One

type IdentityFree a = Term Zero a 
type MaybeFree a = Term One a

-- Commands
data Incr t = Incr Int t
instance Functor Incr where
  fmap f (Incr i t) = Incr i $ f t

data Recall t = Recall (Int -> t)
instance Functor Recall where
  fmap f (Recall g) = Recall (f . g)


inject :: (g :≺: f) => g (Term f a) -> Term f a
inject = Impure . inj

incr :: (Incr :≺: f) => Int -> Term f ()
incr i = inject (Incr i (Pure ()))

recall :: (Recall :≺: f) => Term f Int
recall = inject (Recall Pure)

tick :: Term (Recall :+: Incr) Int
tick = do
  y <- recall
  incr 1
  return y



foldTerm :: Functor f => (a -> b) -> (f b -> b) -> Term f a -> b
foldTerm pure imp (Pure x) = pure x
foldTerm pure imp (Impure t) = imp (fmap (foldTerm pure imp) t)

-- Handler
class Functor f => Run f where
  runAlgebra :: f (Mem -> (a, Mem)) -> (Mem -> (a, Mem))

newtype Mem = Mem Int

instance Run Incr where
  runAlgebra (Incr k r) (Mem i) = r (Mem (i + k))

instance Run Recall where
  runAlgebra (Recall r) (Mem i) = r i (Mem i)

instance (Run f , Run g) => Run (f :+: g) where
  runAlgebra (Inl r) = runAlgebra r
  runAlgebra (Inr r) = runAlgebra r

run :: Run f => Term f a -> Mem -> (a , Mem)
run = foldTerm ( , ) runAlgebra

-- run tick (Mem 4)
-- >> (4 , Mem 5)


data (f :+: g) e = Inl (f e) | Inr (g e)
instance (Functor f , Functor g) => Functor (f :+: g) where
  fmap f (Inl e) = Inl $ fmap f e
  fmap f (Inr e) = Inr $ fmap f e

class (Functor f , Functor g) => f :≺: g where
  inj :: f a -> g a
-- refl
instance Functor f => f :≺: f where
  inj = id
-- addright
instance (Functor f , Functor g ) => f :≺: (f :+: g) where
  inj = Inl
-- addleft
instance (Functor f , Functor g, Functor h, f :≺: g) => f :≺: (h :+: g) where
  inj = Inr . inj
