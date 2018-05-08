This is literate haskell, which contains specifications for a function 
guessCoordinateMatrix 

\begin{code}
module GuessCoordinateMatrixSpec where
import Test.Hspec

import Types
import Matrix as M
import TestEnv
import CTM 
  ( guessCoordinateMatrixTransform
  , normal
  , swapped
  , invert
  , matToPoint
  , translatePoint
  )
\end{code}

1. Intro
  This particular module contains specs for function 
guessCoordinateTransformMatrix.
  guessCoordinateTransformMatrix:: Geometry-> [(Int,Int)]-> m Matrix
  guessCoordinateTransformMatrix geometry points
  This is the main function here. It's purpose is to decide if input device
needs some calibration corrections in order to work as expected. If it decides,
that device needs corrections, it will generate appropriate Coordinate Matrix
Transformation, which can be used to pass to `xinput` binary.

  Here a brief about Coordinate Transformation Matrix (the detailed is here
https://wiki.ubuntu.com/X/InputCoordinateTransformation).
Suppose, that we have touched the touchscreen and xinput gave us swapped coords
(x=197,y=400), but the correct ones are: (x=400,y=197). So the axes are swapped!
Default Coordinate Transformation Matrix is (3x3) identity matrix, so xinput
will multiple it on touch coordinates:

⎡ 1 0 0 ⎤   ⎡ 197 ⎤   ⎡ 197 ⎤
⎜ 0 1 0 ⎥ · ⎜ 400 ⎥ = ⎜ 400 ⎥
⎣ 0 0 1 ⎦   ⎣  1  ⎦   ⎣  1  ⎦

In order to swap axes, we need to change matrix so:

⎡ 0 1 0 ⎤   ⎡ 197 ⎤   ⎡ 0x197+1x400+0x1 ⎤   ⎡ 400 ⎤
⎜ 1 0 0 ⎥ · ⎜ 400 ⎥ = ⎜ 1x197+0x400+0x1 ⎥ = ⎜ 197 ⎥
⎣ 0 0 1 ⎦   ⎣  1  ⎦   ⎣ 0x197+0x400+1x1 ⎦   ⎣  1  ⎦

  The same method should be used in order to invert axes

1.1 Scaling

Reference URL: `https://wiki.archlinux.org/index.php/Calibrating_Touchscreen#Calculate_the_Coordinate_Transformation_Matrix`
In order to scale device's coordinates to fir Xserver's coordinates, we need to
have a matrix:
[ c0 0  c1 ]
[ 0  c2 c3 ]
[ 0  0  1  ]

where
c0 = `touch_area_width` / `total_width`
c2 = `touch_area_height` / `total_height`
c1 = `touch_area_x_offset` / `total_width`
c3 = `touch_area_y_offset` / `total_height`
x and y offset are (x,y) coordinates of the left top touch point


2. Tests and specs

We will run all our test supposing, that we have display with resolution 800x600
So let's define it as common name:

\begin{code}
g = Geometry 800 600
\end{code}

2.1. Lets suppose, that we have screen with size 1024x768 and touchscreen with the
same coordinates values. No axes swap, no inversion and you had touched points
almost at the corners (difference <= 2%). This should give us just identity
matrix

\begin{code}
test1 = it "no inversions, no swap, no scale: should return default matrix" $ do
    let matrix = runTestEnv defWorld $ guessCoordinateMatrixTransform g
          [Point2 100 75, Point2 700 75, Point2 100 525, Point2 700 525]
    matrix `shouldReturn` normal
\end{code}

2.2. this test shows, the real data from our real device, which has: 1. axis
inversion; 2. swapped axes; 3. touch's coordinates are misscaled.

\begin{code}
test2 = it "inversion, swap and scale" $ do
  let matrix = runTestEnv defWorld $ guessCoordinateMatrixTransform g points
  matrix `shouldReturn` result 
    where
      points = [Point2 584 449, Point2 601 147, Point2 196 444, Point2 206 144]
\end{code}

calibrate matrix is folded from 1.1

The difficulty with inversioned coordinates is that we required to translate them
first so they will be positive before doing swap. Otherwise, if we will be
required to swap screen's size (g binding above) from 800x600 to 600x800.
At the moment, this is the best approach, that I see. 
TODO: think of better approach of handling translattions

Translation of coordinates is needed, because we need to work with real values,
which are positive.

\begin{code}
      (p0:p1:p2:p3:_) = map (translatePoint g (matProduct swapped invert)) points
      Geometry w h = g
      result = M.fromList [[0.0,-1.4754097,1.2516667],[-1.4814814,0.0,1.24875],[0.0,0.0,1.0]]
\end{code}


2.3. swap, invert and scale one more time

\begin{code}
test3 = it "swap, invert and scale one more time" $ do
  let matrix = runTestEnv defWorld $ guessCoordinateMatrixTransform g points
  matrix `shouldReturn` result 
    where
      points = [Point2 586 451, Point2 589 147, Point2 217 448, Point2 199 145]
      result = M.fromList
        [ [ 0, -1.4705882,1.2483333]
        , [-1.5384616,0.0,1.2637501]
        , [ 0, 0, 1]
        ]
\end{code}

`spec` is the main function here. It contains list of subfunctions, that
are contain actuall test code.

\begin{code}
spec = describe "guessCoordinateTransformationMatrix" $ do
  test1
  test2
  test3

\end{code}



