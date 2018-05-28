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
https://wiki.ubuntu.com/X/InputCoordinateTransformation). You can read it, but
you have to admit, that this documenation is not full and not covers all the
cases.

  In order to generate proper Coordinate Transformation Matrix (CTM), I had to
rethink those docs to fit more cases.
  One biggest difference from those docs is that coordinates are normalized. All
operations with matrixes are done with normalized coordinates. Only in this case
it does makes sense.
  The problem.
  You have touchscreen, that is connected, working, but when you touch it, it
"touches" unexpected places.
  The source.
  Touchscreen sends data in it's own coordinate system and X server
is not really aware of it.
  The solution
  By adjusting CTM, we can say X server to transform touchscreen's
coordinates into X server's coordinates. In order to do that, you need to touch
some fixed points in X server's coordinate system. We will call such points as
`desired points` (dp0-dp3). Usually, they are screen's corners, in this case
dp0 is (0,0), dp1 is (1,0), dp2 is (0,1) and dp3 is (1,1). But for now we are
using `xinput_calibrator` as frontend and it divides width and height to 8
blocks so you are touching not corners of the screen, but corners of blocks, so
dp0 is (0.125,0.125) and dp3 is (0.875,0.875). So when we have 4 normalized
points from touch screen p0..p3, we need such CTM, that will transform p0 into
dp0, p1 into dp1, p2 into dp2 and p3 into dp3.
  Algorithm.
  1. run `xinput_calibrator` with args --fake and -v, which will tell it to:
    a. to not touch real device;
    b. log touch points into stdout.
    So by parsing it's outputs, we will have p0..p3 and dp0..dp3
  2. determine screen's size (by running xrandr);
  3. normalize points to have np0..np3
  4. find rotation matrix. This is done by multiplication of rotate matrixes to
     the np0..np3 to get translated normalized points tnp0..tnp3. The result
     rotation matrix is that tnp0..tnp3 are closest points to dp0..dp3 from all
     of results;
  5. find horizontal scale c0=TAWI/TAWA and vertical scale c2=TAHI/TAHA, where
     TAWI - ideal touch area width. In our case it is (0.875-0.125)=0.75
     TAWA - actual touch area width=(max tnp1(x) tnp3(x))-(min tnp0(x) tnp2(x))
     TAHI - ideal touch area height. In our case is 0.75
     TAHA - actual touch area height. The same as TAWA but for y
  6. find X and Y offset
     ⎡ -c0 0 dp0(x) ⎤   ⎡ tnp0(x) ⎤   ⎡ c1 ⎤
     ⎜ 0 -c2 dp0(y) ⎥ · ⎜ tnp0(y) ⎥ = ⎜ c3 ⎥
     ⎣ 0   0   1    ⎦   ⎣    1    ⎦   ⎣ 1  ⎦
     where
       c1 - X-offset
       c3 - Y-offset
  7. build scale matrix, which is:
     ⎡ c0 0 c1 ⎤
     ⎜ 0 c2 c3 ⎥
     ⎣ 0  0  1 ⎦
  8. find CTM by multiplication of scale matrix to rotation:
     ⎡ c0 0 c1 ⎤   ⎡          ⎤
     ⎜ 0 c2 c3 ⎥ · ⎜ rotation ⎥
     ⎣ 0  0  1 ⎦   ⎣          ⎦


2. Tests and specs

We will run all our test supposing, that we have display with resolution 800x600
So let's define it as common name:

\begin{code}
g = Geometry 800 600
\end{code}

2.1. Lets suppose, that we have screen with size 800x600 and touchscreen with the
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

the result is taken from guessCoordinateMatrixTransform, but it is very close to
those values, that had been calculated manually, just have more precision.

\begin{code}
      result = M.fromList
        [ [0.0,-1.4754099,1.2290983]
        , [-1.4814814,0.0,1.237963]
        , [0.0,0.0,1.0]
        ]
\end{code}


2.3. swap, invert and scale one more time

\begin{code}
test3 = it "swap, invert and scale one more time" $ do
  let matrix = runTestEnv defWorld $ guessCoordinateMatrixTransform g points
  matrix `shouldReturn` result
    where
      points = [Point2 586 451, Point2 589 147, Point2 217 448, Point2 199 145]
      result = M.fromList
        [ [0.0,-1.4705882,1.2303921]
        , [-1.5384614,0.0,1.2576922]
        , [ 0, 0, 1]
        ]
\end{code}

2.4 swapm invert and scale

This is mocked example, that has precise clicks and suppose to have scale on
both axes to be 1.5 and both coordinates offset of 0.125:

\begin{code}
test4 = it "mocked invert, swap and scale" $ do
  let matrix = runTestEnv defWorld $ guessCoordinateMatrixTransform g points
  matrix `shouldReturn` result
    where
      points = [Point2 800 600, Point2 800 300, Point2 400 600, Point2 400 300]
      result = M.fromList
        [ [ 0.0, -1.5, 1.625]
        , [ -1.5, 0.0, 1.625]
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
  test4

\end{code}
