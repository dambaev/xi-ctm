module Vector where

import Types

vector
  :: Point2
  -> Point2
  -> Vector2
vector (Point2 x0 y0) (Point2 x1 y1) = Vector2 (x1 - x0) (y1 - y0)

lengthV
  :: Vector2
  -> Float
lengthV (Vector2 x y) = sqrt( x*x + y*y)

lengthP:: Point2-> Point2-> Float
lengthP p0 p1 = lengthV $ vector p0 p1
