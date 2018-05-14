module Vector where

import Types

vector
  :: (Num a)
  => Point2 a
  -> Point2 a
  -> Vector2 a
vector (Point2 x0 y0) (Point2 x1 y1) = Vector2 (x1 - x0) (y1 - y0)

lengthV
  :: (Floating a)
  => Vector2 a
  -> a
lengthV (Vector2 x y) = sqrt( x*x + y*y)

lengthP
  :: ( Num a
     , Floating a
     ) 
  => Point2 a
  -> Point2 a
  -> a
lengthP p0 p1 = lengthV $ vector p0 p1
