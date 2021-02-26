-- Applicative
-- Maybe a や IO a は「文脈の付加された値」
-- Applicativeは文脈を保ったまま、通常の関数を適用されることができる

-- Monad
-- Applicative値の拡張
-- (a -> M b)の関数に(M a)の値を渡したい

-- **bind**
-- (>>=') :: Monad m => m a -> (a -> m b) -> m b


class Monad' m where
  return' :: a -> m a
  (>>=')  :: m a -> (a -> m b) -> m b
  (>>')    :: m a -> m b -> m b
  x >>' y = x >>= (\_ -> y)
  fail' :: String -> m a
  fail' msg = error msg

instance Monad Maybe where
  return x = Just x
  Nothing >>= f = Nothing
  Just x >>= f = Just (f x)
  fail _ = Nothing


-- return x >>= f >>= g >>= h ...
-- Applicativeなら、 h <$> (g <$> (f <$> pure x))
-- これでは(f <$> pure x)の結果はNothingになり得ない。(fはa -> b)
-- しかし(x >>= f)のfは a -> Maybe a なのでNothingも表現できる
-- パターンマッチが不要(感動)
-- この点においてApplicativeより強力

-- (Nothing >> Just 3) == Nothing
-- (Just undefined >> Just 5) == Just 5

-- doはモナド全体のための構文
foo :: Maybe String
foo =
  Just 3 >>= \x ->
  Just "!" >>= \y ->
  Just ((show x) ++ y)

bar :: Maybe String
bar = do
  x <- Just 3
  y <- Just "!"
  return ((show x) ++ y)

-- モナド則
-- 左恒等性 : (return x >>= f) == return (f x)
-- 右恒等性 : (m >>= return) == m
-- 結合法則 : ((m >>= f) >== g) == m >>= (\x -> (f x) >>= g)



-- t . t . (id, return) == return . t
-- t . (id, return) == return

data Either' a b = Left' a | Right' b
instance Monad (Either' e) where
  return x = Right' x
  (Left a) >>= _ = Left' a
  (Right b) >>= f = f b

-- モナド変換子
data Identity a = id a
runIdentity (id x) = x

-- StateT s m t
newtype StateT s m a = StateT { runStateT :: (s -> m (a , s)) }
type State' s a = StateT s Identity a

-- runStateT :: State s m a -> s -> m (a , s)
-- unStateと読める

st = return 1 :: State s Int     -- Stateを生成

f1 :: s -> (Int, s)
f1 = runState st                 -- 加工された内部関数を取り出し

f2 :: s -> Identity (Int, s)
f2 = runStateT st                -- 生の内部関数を取り出し

-- 同じ値を出力
test = do
    print $ f1 ()                -- (値, 状態)
    print $ runIdentity $ f2 ()  -- Identityで包まれている

-- 値だけ: evalStateT :: (Monad m) => StateT s m a -> s -> m a
-- 状態だけ: execStateT :: (Monad m) => StateT s m a -> s -> m s

test2 = do
    let st = return 1 :: StateT s Identity Int
    print $ runIdentity $  runStateT st ()      -- (値, 状態)
    print $ runIdentity $ evalStateT st ()      --  値
    print $ runIdentity $ execStateT st ()      --      状態

-- 生成関数
stateT :: (s -> m (a,s)) -> StateT s m a

test3 = do
    let st = StateT $ \s -> Identity (1, s)  -- 関数から生成
    print $ runIdentity $ runStateT st ()    -- StateTとして評価
    print $ runState st ()                   -- Stateとして評価

-- Stateモナドの各種関数の実装
-- return :: a -> State s a
-- return x = stateT $ \s -> Identity (x , s)

-- runState :: StateT s a -> s -> (a , s)
-- runState st s = runIdentity $ runStateT st s

-- state :: (s -> (a , s)) -> State s a
-- state f = StateT $ \s -> Identity (f s)

-- get :: State s s
-- get = state $ \s -> (s , s)

-- set :: a -> State a a
-- set a = state $ \s -> (a , a)

-- >>= :: State s a -> (a -> State s a) -> State s a
-- st >>= f = state $ \s ->
--  let (a , s') = (runState st s) in
--    (runState (f a) s')




