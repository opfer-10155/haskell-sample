data Free f a =
  Pure a |
  Impure (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure x)   = Pure $ f x
  fmap f (Impure c) = Impure $ fmap (fmap f) c

-- ファンクタfをモナドにする高階関手
instance Functor f => Monad (Free f) where
  return = Pure
  Pure x >>= f = f x
  Impure c >>= f = Impure $ fmap (>>= f) c

foldTerm :: Functor f =>
            (a -> r) -> (f r -> r) -> Free f a  -> r
foldTerm ret handle (Pure x) = ret x
foldTerm ret handle (Impure c) = handle (fmap (foldTerm ret handle) c)


data Result a = Ok a | Error

instance Functor Result where
  fmap f (Ok x) = Ok $ f x
  fmap f Error = Error


t1 :: Free Result Int
t1 = Impure (Ok (Pure 1)) >>= \_ -> Impure (Ok (Pure 2))
-- Impure $ fmap (>>= \_ -> Impure (Ok (Pure 2))) (Ok (Pure 1))
-- Impure $ Ok (Pure 1 >>= \_ -> Impure (Ok 2))
-- Impure $ Ok (Pure 1 >>= \_ -> Impure (Ok 2))
-- Impure $ Ok $ (\_ -> Impure (Ok 2)) 1
-- Impure $ Ok (Impure (Ok 2))


data Cont r r' a = Cont {runCont :: (a -> r) -> r'}

instance Functor (Cont r r') where
  fmap k c = Cont $ \k' -> runCont c (k' . k)

