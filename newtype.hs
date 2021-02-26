-- newtypeはdataより高速であるが、値コンストラクタは1つしか定義できない

data ZipList a = ZipList {getZipList :: [a]} deriving(Eq)

newtype ZipList' a = ZipList' {getZipList :: [a]} deriving(Eq)

