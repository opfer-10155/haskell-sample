import Control.Monad

main = forever $ do
  putStr "Input: "
  l <- getLine
  putStrLn $ "echo " ++ Input

-- forever IOアクションを無限に繰り返す
