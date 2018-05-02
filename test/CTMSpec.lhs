This is literate haskell

\begin{code}
module CTMSpec where
import Test.Hspec
import Data.Text as T
import Prelude as P hiding
  ( getContents
  )
import Types
import Matrix as M

import Interface
import TestEnv
import CTM

\end{code}

1. Intro


2. Tests and specs

This input data will be used in testing

\begin{code}
inputDataBad = T.unlines 
  [ "DEBUG: Adding click 0 (X=584, Y=449)"
  , "DEBUG: Adding click 1 (X=601, Y=147)"
  , "DEBUG: Adding click 2 (X=196, Y=444)"
  , "DEBUG: Adding click 3 (X=206, Y=144)"
  , "DEBUG: Adding click 4 (X=0, Y=0)"
  ]
inputDataOk = T.unlines $ P.take 4 $ T.lines inputDataBad
\end{code}

2.1 matrixToXinputProp

\begin{code}
test1 = it "matrixToXinputProp should return matrix as string" $ do
  matrixToXinputProp matrix `shouldBe` matrixStr
  where
    matrix = M.fromList [[1,2,3],[4,5,6],[7,8,9]]
    matrixStr = "1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0"
\end{code}

2.2 extractPoints

2.2.1 We only need 4 points

\begin{code}
test2 = it "we should not handle more then 4 clicks" $ do
  let world = setStdin inputDataBad defWorld 
      result = runTestEnv world $ do
          input <- getContents
          extractPoints $ T.lines input
  result `shouldThrow` anyException
  where
    points = [ Point2 584 449
             , Point2 601 147
             , Point2 196 444
             , Point2 206 144
             ]
\end{code}

\begin{code}
test3 = it "we should not handle 3 clicks" $ do
  let world = setStdin (T.unlines $ P.drop 1 $ T.lines $ inputDataOk) defWorld
      result = runTestEnv world $ do
          input <- getContents
          extractPoints $ T.lines input
  result `shouldThrow` anyException
\end{code}

2.2.2 We should be able to read points from `xinput_calibrator`

\begin{code}
test4 = it "should read 4 points from stdin" $ do
  let world = setStdin inputDataOk defWorld 
      result = runTestEnv world $ do
          input <- getContents
          extractPoints $ T.lines input
  result `shouldReturn` points
  where
    points = [ Point2 584 449
             , Point2 601 147
             , Point2 196 444
             , Point2 206 144
             ] 
\end{code}

2.3 matProduct

test matrix multiplication

\begin{code}
test5 = it "matrix multiplication" $ do
  matProduct matA matB `shouldBe` result
  where
    matA = M.fromList
      [ [ 1, 2]
      , [ 3, 4]
      ]
    matB = M.fromList
      [ [ 2, 3]
      , [ 4, 5]
      ]
    result = M.fromList 
      [ [ 10, 13]
      , [ 22, 29]
      ]
\end{code}

\begin{code}
spec = describe "CTM module tests" $ do
  test1
  test2
  test3
  test4
  test5
\end{code}
