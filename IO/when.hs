import Control.Monad

main = do
  input <- getLine
  when (input == "unko") $ do
    putStrLn "汚ねえ"

-- when 条件がtrueなら2引数目のIOを実行 falseならreturn ()する
-- return :: a -> IO a
-- return x = IO x
