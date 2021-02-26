import Text.Paesec

-- バックトラック
test9 = sequence [char 'a', char 'b']  -- 'a'が成功して'b'で失敗したらエラー
    <|> sequence [char 'a', char 'c']

main = do
    parseTest test9 "ab" -- "ab"
    parseTest test9 "ac" -- error

-- 1文字ずつcharでパースしなくても、文字列で指定できるstringがあります。
-- ただし内部では1文字ずつ処理されているため、途中の失敗をバックトラックするにはtryが必要です。


-- 四則演算の構文解析

data AST =
  Num Int |
  Op (Int -> Int -> Int) AST AST


-- 数値トークン?
number = do
  x <- many1 digit -- 1文字以上の数字 (manyは0文字以上)
  return $ Num (read x :: Int)

-- 二項演算
binOp = do
  x  <- number


-- 項(積、商)
term = do
    x  <- number
    fs <- many $ do
            char '*'
            y <- number
            return (\t -> Op (*) t y)
        <|> do
            char '/'
            y <- number
            return (\t -> Op div t y)
    return $ foldl (\x f -> f x) x fs

-- 式
expr = do
    x  <- term                         -- 項を取得
    fs <- many $ do
            char '+'
            y <- term                  -- 項を取得
            return (\e -> Op (+) e y)
        <|> do
            char '-'
            y <- term                  -- 項を取得
            return (\e -> Op (-) e y)
    return $ foldl (\x f -> f x) x fs

