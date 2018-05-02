module Matrix where

import Prelude as P
import Data.Vector as V
import Data.List hiding (transpose)
import Types


{-@ matProduct 
      :: a:MatrixN v Pos Pos 
      -> b:MatrixN v Pos Pos 
      -> { c:MatrixN v Pos Pos | mCol a = mRow b && mRow c = mCol a && mCol c = mCol b
         }
@-}
matProduct a@(M ar ac as) b@(M br bc _) = M cr cc cs
  where
    cr = ac
    cc = bc
    (M _ _ bs) = transpose b
    cs = for as $ \ai->
          for bs $ \bj->
            dotProduct ai bj

{-@ transpose:: m:MatrixN a Pos Pos -> {v:MatrixN a Pos Pos | mRow m = mCol v && mCol m = mRow v} @-}
transpose (M r c s) = M c r bs
  where
    bs = loop V.empty s
    loop tmp s 
      | V.null (V.head s) = tmp
    loop tmp s = loop newtmp news
      where
        newtmp = tmp `V.snoc` oldrow
        oldrow = V.map V.head s
        news = V.map V.tail s

        
{-@ dotProduct:: m:VectorN a Pos -> n:VectorN a Pos-> {v:a | vlen m == vlen n} @-}
dotProduct v1 v2 = P.sum $ V.zipWith (*) v1 v2



{-@ for:: v:Vector a-> (a -> b) -> VectorN b {vlen v} @-}
for v f = V.map f v

{-@ fromList :: v:[[a]]-> { m:Matrix a | vlen v == mRow m && vlen (head v)
== mCol m} @-}
fromList:: [[a]] -> Matrix a
fromList [] = error "there is no possible matrix from empty list"
fromList rcs 
  | P.all (==c) $ P.map P.length rcs = loop (M r c V.empty) rcs
  | otherwise = error "all rows should have the same columns"
  where
    r = P.length rcs
    c = P.length $ P.head rcs
    loop tmp [] = tmp
    loop tmp curs = loop tmp{ mEls = newVec} news
      where
        (oldcol:news) = curs
        newVec = mEls tmp `V.snoc` V.fromList oldcol

instance (Show a)=> Show (Matrix a) where
    show m = show els
      where
        els = P.map V.toList $ V.toList  $ mEls m
