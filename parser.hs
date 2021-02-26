import Data.Char
import Control.Monad
import Control.Monad.State

-- パーサコンビネータ

-- モナドなし版

-- 任意の1文字を取り出す
anyChar_1 (x:_) = x

-- 残りの文字列も保持するバージョン
anyChar_2(x:xs) = (x, xs)

-- 2文字取得
get2 xs =
  let
    (x1, xs1) = anyChar_2 xs
    (x2, xs2) = anyChar_2 xs1
  in
    ([x1, x2], xs2)

-- satisfy ... 条件を満たす先頭1文字を抽出
satisfy :: (Char -> Bool) -> String -> Maybe (Char , String)
satisfy f (x:xs) = if (f x) then return (x , xs) else Nothing


-- satisfyを使ったパーサ

char c = satisfy (== c)
digit  = satisfy isDigit
letter = satisfy isLetter


-- Stateモナドを使う(すでにある状態をどういじるかをイメージして実装する)

anyChar_s :: State String Char
anyChar_s = state $ (\(x:xs) -> (x , xs))

satisfy_s :: (Char -> Bool) -> State String Char
satisfy_s f = state $ \(x:xs) -> if (f x) then (x , xs) else (x , "")

get2s = sequense [anyChar , anyChar]
-- get2s "abc" => ("ab" , "c")


-- Maybeモナド
anyChar_m :: String -> Maybe (Char , String)
anyChar_m [] = Nothing
anyChar_m (x:xs) = return (x , xs)

satisfy_m :: (Char -> Bool) -> String -> Maybe (Char , String)
satisfy_m f [] = Nothing
satisfy_m f (x:xs) = if (f x) then return (x , xs) else Nothing


-- Eitherモナド(失敗の理由を記述できる)

anyChar_e :: String -> Either String (Char , String)
anyChar_e [] = Left "Empty String"
anyChar_e (x:xs) = return (x , xs)

satisfy_e :: (Char -> Bool) -> String -> Either String (Char , String)
satisfy_e f [] = Left "Empty String"
satisfy_e f (x:xs) = if (f x) then return (x , xs) else Left "Unsatisfying"

Left a <|> Left b = Left $ b ++ a
Left _ <|> b      = b
a      <|> _      = a

char_e c xs = satisfy_e (== c)   xs <|> Left ("not char " ++ show c)
digit_e  xs = satisfy_e isDigit  xs <|> Left "not digit"
letter_e xs = satisfy_e isLetter xs <|> Left "not letter"


-- モナド変換子
anyChar_st :: StateT s (Either String) a
anyChar_st = StateT $ solve where
  solve [] = Left "Empty"
  solve (x:xs) = Right (x , xs)

satisfy_st :: (Char -> Bool) -> StateT s (Either String) a
satisfy_st f = StateT $ solve where
  solve [] = Left "Empty"
  solve (x:xs) = if (f x) then Right (x , xs) else Left "Unsatisfying"


(StateT f) <|> (StateT g) = StateT $ \s ->
  (f s) <|>' (g s) where
    (Left a) <|>' (Left b) = Left $ b ++ " " ++ a
    (Left _) <|>' (Right b) = b
    (Right a) <|>' (Left _) = a

parseTest :: StateT s (Either String) a -> String -> IO b
parseTest p s = case evalStateT p s of
    Right r -> print r
    Left  e -> putStrLn $ "[" ++ show s ++ "] " ++ e

main = do
  print "abc"
