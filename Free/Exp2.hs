{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


data Exp f = In (f (Exp f))

data Val e = Val Int
type IntExp = Exp Val

-- e : expressions as subtype
data Add e = Add e e
type AddExp = Exp Add

-- IntExp と AddExp を結合したような型が1つあれば解決

-- fとgのco-product
data (f :+: g) e = Inl (f e) | Inr (g e)

-- Exp1のExpと同型
type Term = Exp (Val :+: Add)

t1 :: Term
t1 = In (
  Inr (Add
    (In (Inl (Val 1)))
    (In (Inl (Val 2)))
  ))


-- Interpretation
instance Functor Val where
  fmap f (Val x) = Val x

instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor f , Functor g) => Functor (f :+: g) where
  fmap f (Inl e) = Inl $ fmap f e
  fmap f (Inr e) = Inr $ fmap f e

-- f : f-algebra . It specifies one step of recursion, turning a value of type f a into the desired result a
-- The fold itself uniformly applies these operations to an entire expression.
foldExp :: Functor f => (f a -> a) -> Exp f -> a
foldExp f (In e) = f $ fmap (foldExp f) e

class Functor f => Eval f where
  evalAlg :: f Int -> Int


instance Eval Val where
  evalAlg (Val x) = x

instance Eval Add where
  evalAlg (Add x y) = x + y

instance (Eval f , Eval g) => Eval (f :+: g) where
  evalAlg (Inl e) = evalAlg e
  evalAlg (Inr e) = evalAlg e

eval :: Term -> Int
eval = foldExp evalAlg

-- A is subset of B <==> exist inj : A -> B ; inj = id
-- class Subset a b where
--   inj :: a -> b

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


