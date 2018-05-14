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

newtype NFloat = NFloat Float
                 deriving (Eq,Ord)
instance Show NFloat where
    show (NFloat v) = show v
instance Num NFloat where
  (+) (NFloat a) (NFloat b) = NFloat (a + b)
  (*) (NFloat a) (NFloat b) = NFloat (a * b)
  abs (NFloat a) = NFloat (abs a)
  signum (NFloat a) = NFloat (signum a)
  fromInteger a = NFloat (fromInteger a)
  negate (NFloat a) = NFloat (negate a)

instance Fractional NFloat where
    fromRational a = NFloat (fromRational a)
    (/) (NFloat a) (NFloat b) = NFloat (a/b)
instance Floating NFloat where
    pi = NFloat pi
    exp (NFloat a) = NFloat (exp a)
    log (NFloat a) = NFloat (log a)
    sin (NFloat a) = NFloat (sin a)
    cos (NFloat a) = NFloat (cos a)
    acos (NFloat a) = NFloat (acos a)
    asin (NFloat a) = NFloat (asin a)
    atan (NFloat a) = NFloat (atan a)
    sinh (NFloat a) = NFloat (sinh a)
    cosh (NFloat a) = NFloat (cosh a)
    asinh (NFloat a) = NFloat (asinh a)
    acosh (NFloat a) = NFloat (acosh a)
    atanh (NFloat a) = NFloat (atanh a)

data Point2 a = Point2 a a
            deriving (Show,Eq)

data Vector2 a = Vector2 a a
             deriving (Eq,Show)

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


