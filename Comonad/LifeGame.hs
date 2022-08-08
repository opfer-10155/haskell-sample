import Zipper
import Comonad
-- 誕生：死んでいるセルに隣接する生きたセルが丁度三つあれば、次の世代は誕生する。
-- 生存：生きているセルに隣接するセルが二つか三つならば、次の世代は生存する。
-- 過疎：生きているセルに隣接する生きたセルが一つ以下ならば、過疎により死滅する。
-- 過密：生きているセルに隣接するセルが四つ以上ならば、過密により死滅する。


newtype Zipper2D a = Z2 (Zipper (Zipper a))

instance Functor Zipper2D where
  fmap f (Z2 zz) = Z2 (fmap (fmap f) zz)

instance Comonad Zipper2D where
  extract (Z2 zz) = extract (extract zz)
  duplicate (Z2 zz) = fmap Z2 . Z2 . roll $ roll zz where
    -- ２重になったZipperの内側を左右にずらしたものを集めてより大きなZipperを作る。
    roll :: Zipper (Zipper a) -> Zipper (Zipper (Zipper a))
    roll zz = Z (iterate1 (fmap left) zz) zz (iterate1 (fmap right) zz)


type World = Zipper2D Bool

-- Worldが注目しているマスの隣り合わせのマスの生存数
countNeighbours :: World -> Int
countNeighbours (Z2 (Z
  ((Z (upl:_) up (upr:_)):_)
  (Z (l:_)   _  (r:_) )
  ((Z (lol:_) lo (lor:_)):_)))
  = length $ filter id [upl, up, upr, l, r, lol, lo, lor]


-- @returns : Worldが注目しているマスの次の状態
life :: World -> Bool
life w =
  let a = extract w in -- 注目しているマス
  let n = countNeighbours w in
  (a && (n == 2 || n == 3)) || (not a && n == 3)

-- ライフゲームを1step進める
lifegame :: World -> World
lifegame = extend life

