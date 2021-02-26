-- dataもexport可能 --
module Shapes (
  Shape(..) --コンストラクタ全てexport
) where

-- 型定義 --
data Bool' = True | False
{-
  右辺を値コンストラクタという
  その型のとりうる値の種類を指定
-}

-- 型自体も値コンストラクタにできる --
-- 違う、Num'型のIntという値ができただけだ --
data Num' = Int | Integer


-- data KeyValue = Int -> String ダメらしい

data Shape =
  Circle Float Float Float |
  Rectangle Float Float Float Float

{- scalaで書くところの
trait Shape
case class Circle(x: Float, y: Float, r: Float) extends Shape
case class Reactange(x1: Float, y1: Float, x2: Float, y2: Float) extends Shape

ただし、Circle x y rやReactange x1 y1 x2 y2はShape型の`値`ということに注意
Circle, Reactangeを型として扱うことはできない
-}
{-
> :t Circle
Circle :: Float -> Float -> Float -> Shape
> :t Rectangle
Rectangle :: Float -> Float -> Float -> Float -> Shape
-}

-- 面積
area :: Shape -> Float
area (Circle _ _ r) = pi * r^2
area (Rectangle x1 y1 x2 y2) = abs ((x2 - x1) * (y2 - y1))

cirles = map (\r -> Circle 0 0 r) [1,2,3,4]

-- レコード構文
data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int,
  height :: Float
} deriving (Show, Eq) -- ShowクラスとEqのインスタンスになる、show関数と比較関数が自動で定義される

guy = Person "opf" "er" 21 165
name = firstName guy ++ lastName guy

gay = Person {
  firstName="senpai",
  lastName="yaju",
  age=24,
  height=170
}

nameOf :: Person -> String
nameOf (Person{firstName=fn, lastName=ln, age=_, height=_}) = fn ++ ln

-- 型引数、型コンストラクタ
data Maybe' a = Nothing | Just a

{-
  型引数はa、Maybe'が型コンストラクタ
  型を実引数にとって、型をつくる
  a=Intとして、Maybe' Intという型ができる
  値Just 1 は (Num a) => Maybe' aと推論される

  具体型: 型引数をとらない型(Int) or 型引数をすべて埋めた型(Maybe Int)
  抽象型: 具体型ではないもの？
-}

-- 型シノニム: 型のエイリアス
-- 型コンストラクタとの違いは新しい型を作っているわけではないところ
type Hoge = Int -- | String  ダメだった…
-- 型引数もとれる
type AssocList k v = [(k, v)]
-- 型引数は部分適用もアリ、型コンストラクタも同じ
type MapToInt k = AssocList k Int
-- union型: aかb
type Union = Either Int String
data Either' a b = Left a | Right b
-- 左にエラー、右に正しい返り値という風習が

-- 再帰
data List a = Nil | Cons a (List a)
  deriving (Show, Read, Eq, Ord)

