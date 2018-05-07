module Types where

import Control.Exception
import Data.Vector as V

data EXrandrFailed = EXrandrFailed
  deriving Show
instance Exception EXrandrFailed

data EDeviceNameNotFound = EDeviceNameNotFound
  deriving Show
instance Exception EDeviceNameNotFound

data ENotEnoughPoints = ENotEnoughPoints
  deriving Show
instance Exception ENotEnoughPoints

data ENoTransformMatrixFound = ENoTransformMatrixFound
    deriving Show
instance Exception ENoTransformMatrixFound

data EXInputFailed = EXInputFailed
      deriving Show
instance Exception EXInputFailed

{-@ type PosF = {v:Float | v > 0.0 } @-}
{-@ type Nat = {v:Int | v >= 0 } @-}
{-@ type Pos = {v:Nat | v > 0 } @-}

{-@ data Geometry = Geometry {w::PosF, h::PosF } @-}
data Geometry = Geometry Float Float

data Point2 = Point2 Float Float
            deriving (Show,Eq)

data Vector2 = Vector2 Float Float
             deriving Show

{- type VectorN a N = { v:Vector a, N:Pos | vlen v == n} -}
{-@ type VectorN a N = {v:Vector a | vlen v = N} @-}
{-@ data Matrix a = M
            { mRow:: Pos
            , mCol:: Pos
            , mEls:: VectorN (VectorN a mCol) mRow
            } 
@-}
data Matrix a = M
              { mRow:: Int
              , mCol:: Int
              , mEls:: Vector (Vector a)
              }
              deriving Eq

{-@ type MatrixN a R C = { v:Matrix a | Dims v R C} @-}
{-@ predicate Dims M R C = mRow M = R && mCol M = C @-}


