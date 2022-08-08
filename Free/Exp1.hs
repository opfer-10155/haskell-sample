-- Expression Problem :
-- 言語Exprに項を追加したい

data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val x) = x
eval (Add x y) = eval x + eval y


