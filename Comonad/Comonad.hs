module Comonad where

class (Functor w) => Comonad w where
  -- returnの双対
  extract :: w a -> a
  -- (>=>) :: (w a -> b) -> (w b -> c) -> (w a -> c)

  -- joinの双対
  duplicate :: w a -> w (w a)
  -- duplicate = extend id

  -- >>=の双対
  -- m >>= k = join $ fmap k m
  extend :: (w b -> a) -> w b -> w a
  extend f = fmap f . duplicate


