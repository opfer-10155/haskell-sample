main = do
  putStrLn "Hello, What's your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name)

-- getLine :: IO String
-- `<-` IOアクションからデータを取り出す
-- do構文 IOアクションを繋げる
-- doはIO a を返さなくてはならない？