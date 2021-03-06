{-# LANGUAGE OverloadedStrings #-}
module CTM where

import qualified Data.String as S
import Prelude as P hiding
  ( lines
  , takeWhile
  , drop
  , dropWhile
  , getContents
  , putStrLn
  )
import Data.List as P
  ( foldl'
  , intercalate
  )
import System.Exit
import Data.Text as T
import qualified Data.Text.IO as T
import Data.Char
import Data.Maybe (fromJust, isNothing)
import Control.Monad.Catch as E
import Control.Monad as C
import Data.Vector as V
import Types
import Interface
import System.Process (shell)
import Vector as V
import Matrix as M
import Debug.Trace as D

realMain
  :: ( Monad m
     , RunsProcess m
     , MonadThrow m
     , Debugged m
     , ReadsStdin m
     , ReadsEnvironment m
     , WritesToHandle m
     , Profiled m
     )
  => m ()
realMain = profile "realMain" $ do
  debug "running in debug mode"
  input <- getContents
  debug $ "got input: " `T.append` input
  name <- getDeviceName
  points <- extractPoints $ lines input
  size <- getDisplayGeometry
  matrix <- guessCoordinateMatrixTransform size points
  applyCTM name matrix
  printConfig name matrix

getDisplayGeometry
  :: ( Monad m
     , RunsProcess m
     , MonadThrow m
     , Debugged m
     , Profiled m
     )
  => m Geometry
getDisplayGeometry = profile "getDisplayGeometry" $ do
    debug "running xrandr"
    (code, out, err) <- readCreateProcessWithExitCode "xrandr" ""
    when (code /= ExitSuccess) $ throwM EXrandrFailed
    extractDisplayGeometry $ lines out

extractDisplayGeometry
  :: ( Monad m
     , Debugged m
     , Profiled m
     )
  => [Text]
  -> m Geometry
extractDisplayGeometry lined = profile "extractDisplayGeometry" $ do
  let resolution = P.foldl' outputFilter T.empty lined 
      w = read $ T.unpack $ T.takeWhile isDigit resolution
      h = read $ T.unpack $ T.takeWhile isDigit $ T.dropWhile (not . isDigit) 
            $ T.dropWhile isDigit resolution
  debug $ "found resolution " `T.append` (T.pack $ show (w,h))
  return $ Geometry w h
  where
    outputFilter:: Text-> Text-> Text
    outputFilter tmp line 
      | T.null tmp && "*" `isInfixOf` line = 
          T.takeWhile isAlphaNum $ T.dropWhile (not . isDigit) line
    outputFilter tmp line = tmp

extractPoints
  :: ( Monad m
     , Debugged m
     , MonadThrow m
     , Profiled m
     )
  => [Text]
  -> m [Point2 Float]
extractPoints lined = profile "extractPoints" $ do
    let points = P.reverse $ P.foldl' inputFilter [] lined
    debug $ "extracted points) = " `T.append` (T.pack $ show points)
    when (P.length points /= 4) $ throwM ENotEnoughPoints 
    return (P.map (\(x,y)-> Point2 x y) points)
  where
    pointPrefix = "DEBUG: Adding click "
    inputFilter points line = 
      case points of
        _ | pointPrefix `isPrefixOf` line ->
          let pointStr = T.drop (T.length pointPrefix + 5) line
              pointXStr = T.takeWhile isDigit pointStr
              pointYStr = T.takeWhile isDigit $ T.dropWhile (not . isDigit) 
                $ T.drop (T.length pointXStr) pointStr
          in ((read $ T.unpack pointXStr, read $ T.unpack pointYStr): points)
        _ -> points

applyCTM
  :: ( Monad m
     , MonadThrow m
     , RunsProcess m
     , Debugged m
     , Profiled m
     )
  => Text
  -> Matrix NFloat
  -> m ()
applyCTM name matrix = profile "applyCTM" $ do
    debug $ "running " `T.append` cmd
    (code,out,err) <- readCreateProcessWithExitCode cmd
                      ""
    when (code /= ExitSuccess) $ throwM EXInputFailed
    where
      cmd = "xinput set-prop \"" 
        `T.append` name 
        `T.append` "\" 'Coordinate Transformation Matrix' " 
        `T.append` matrixToXinputProp matrix


matrixToXinputProp
  :: Matrix NFloat
  -> Text
matrixToXinputProp (M _ _ els) = (T.pack . P.intercalate " " . P.map (P.intercalate " " . P.map show . V.toList) . V.toList) els


normal:: Matrix NFloat
normal = M.fromList
  [ [ 1.0, 0.0, 0.0]
  , [ 0.0, 1.0, 0.0]
  , [ 0.0, 0.0, 1.0]
  ]

swapped :: Matrix NFloat
swapped = M.fromList
  [ [ 0.0, 1.0, 0.0]
  , [ 1.0, 0.0, 0.0]
  , [ 0.0, 0.0, 1.0]
  ]

leftRotate :: Matrix NFloat
leftRotate = M.fromList
  [ [ 0.0,-1.0, 1.0]
  , [ 1.0, 0.0, 0.0]
  , [ 0.0, 0.0, 1.0]
  ]

rightRotate:: Matrix NFloat
rightRotate = M.fromList
  [ [ 0.0, 1.0, 0.0]
  , [-1.0, 0.0, 1.0]
  , [ 0.0, 0.0, 1.0]
  ]

invert:: Matrix NFloat
invert = M.fromList
  [ [-1.0, 0.0, 1.0]
  , [ 0.0,-1.0, 1.0]
  , [ 0.0, 0.0, 1.0]
  ]

-- this matrix can handle inversion of X-only axis
xInvert:: Matrix NFloat
xInvert = M.fromList
  [ [-1.0, 0.0, 1.0]
  , [ 0.0, 1.0, 0.0]
  , [ 0.0, 0.0, 1.0]
  ]

-- this matrix can handle inversion of Y-only axis
yInvert:: Matrix NFloat
yInvert = M.fromList
  [ [ 1.0, 0.0, 0.0]
  , [ 0.0,-1.0, 1.0]
  , [ 0.0, 0.0, 1.0]
  ]




matrixes:: [(Text, Matrix NFloat)]
matrixes = 
    [ ( "normal"
      , normal 
      )
    , ( "left rotate"
      , leftRotate
      )
    , ( "right rotate"
      , rightRotate 
      )
    , ( "swapped"
      , swapped
      )
    , ( "X-inverted"
      , xInvert
      )
    , ( "Y-inverted"
      , yInvert
      )
    ]

isCorrectOrientation
  :: [Point2 NFloat]
  -> [Point2 NFloat]
  -> Bool
isCorrectOrientation corners points@(p0:p1:p2:p3:_) = 
    P.all helper cornersPointsLengths
    where
      cornersPoints = P.zipWith (,) corners points
      cornersPointsLengths = P.map (\(l,r)-> (l,r,V.lengthP l r)) cornersPoints
      helper (corner,point,len) = 
        P.all (\rpoint-> len < V.lengthP corner rpoint) restPoints
        where
          restPoints = P.filter (/=point) points

getCornersDiffs
  :: [Point2 NFloat]
  -> [Point2 NFloat]
  -> [NFloat]
getCornersDiffs corners points = P.map (\(l,r)-> V.lengthP l r) $  P.zipWith (,) corners points

findClosestRotateMatrix
  :: [Point2 NFloat]
  -> [Point2 NFloat]
  -> Maybe ( [Text]
           , Matrix NFloat
           )
findClosestRotateMatrix corners points = 
    case findTheBestOption totalOptions of
      [] -> Nothing
      ((names, mat):_) -> Just (names, mat)
  where
    totalOptions = normalTry P.++ invertedTry
    normalTry = check ["normal"] normal 
    invertedTry = check ["inverted"] invert
    findTheBestOption options = P.foldl' helper [] options
    helper [] (name, mat) = [(name, mat)]
    helper same@(( cname, cmat):_) (name, mat) 
      | P.all (\(l, r)-> l < r) diffs = same
      | otherwise = [(name, mat)]
      where
        diffs = P.zipWith (,) cdiff diff
        cdiff = getCornersDiffs corners ctpoints
        diff = getCornersDiffs corners tpoints
        ctpoints = P.map (translatePoint cmat) points
        tpoints = P.map (translatePoint mat) points
    check names matrix = findTheBestOption correctOrientation
      where
        options = P.map (\(name,mat)-> (name:names, matProduct matrix mat)) matrixes
        correctOrientation = P.filter (\( _, mat)-> isCorrectOrientation corners (P.map (translatePoint mat) points)) options

translatePoint
  :: Matrix NFloat
  -> Point2 NFloat
  -> Point2 NFloat
translatePoint m (Point2 nx ny) = matToPoint $ matProduct m pointM
  where
    pointM = M.fromList [[nx], [ny], [1]]

matToPoint
  :: Matrix a
  -> Point2 a
matToPoint (M r c els)
  | r /= 3 || c /= 1 = error "matToPoint only can be applied to 3x1"
  | otherwise = Point2 x y
    where
      x = V.head $ els V.! 0
      y = V.head $ els V.! 1

pointToMat
  :: Num a
  => Point2 a
  -> Matrix a
pointToMat (Point2 x y) = M.fromList [[x],[y],[1]]

normalizePoint
  :: Geometry
  -> Point2 Float
  -> Point2 NFloat
normalizePoint (Geometry w h) (Point2 x y) = Point2 (NFloat $ x/w) (NFloat $ y/h)

scalePoint
  :: Geometry
  -> Point2 NFloat
  -> Point2 Float
scalePoint (Geometry w h) (Point2 (NFloat x) (NFloat y)) = Point2 (x*w) (y*h)

calcScaleMatrix
  :: [ Point2 NFloat] -- desired points
  -> [ Point2 NFloat]
  -> Matrix NFloat
calcScaleMatrix dps points = M.fromList
  [ [c0, 0, c1]
  , [ 0,c2, c3]
  , [ 0, 0, 1]
  ]
    where
      Point2 x0' y0':Point2 x1' y1':Point2 x2' y2':Point2 x3' y3':_ = points
      Point2 dp0x dp0y:_:_:Point2 dp3x dp3y:_ = dps
      tawi = dp3x - dp0x
      tahi = dp3y - dp0y
      tawa = x1 - x0
      taha = y1 - y0
      x0 = min x0' x2'
      x1 = max x1' x3'
      y0 = min y0' y1'
      y1 = max y2' y3'
      c0 = tawi / tawa
      c2 = tahi / taha
      Point2 c1 c3 = matToPoint $ matProduct 
        ( M.fromList
        [ [ negate c0, 0, dp0x]
        , [ 0, negate c2, dp0y]
        , [ 0, 0, 1]
        ])
        ( M.fromList
        [ [x0]
        , [y0]
        , [1]
        ])

guessCoordinateMatrixTransform
  :: ( Monad m
     , Debugged m
     , MonadThrow m
     , Profiled m
     )
  => Geometry
  -> [Point2 Float]
  -> m (Matrix NFloat)
guessCoordinateMatrixTransform g@(Geometry w h) points@(p0:p1:p2:p3:_) = profile "guessCoordinateMatrixTransform" $ do
    when ( mClosestTransform == Nothing) $ 
      throwM ENoTransformMatrixFound
    let Just (matrixNames,transformM) = mClosestTransform
        tnpoints = P.map (translatePoint transformM) npoints
        scaleM = calcScaleMatrix corners tnpoints
        result = matProduct scaleM transformM
        rnpoints = P.map (matToPoint . matProduct result . pointToMat) npoints
        rspoints = P.map (scalePoint g) rnpoints
    debug $ "those transformations had been applied " `T.append` (T.pack $ show matrixNames)
    debug $ "translated points are: " `T.append` (T.pack $ show tnpoints)
    debug $ "scale matrix is: " `T.append` (T.pack $ show scaleM)
    debug $ "calibrate matrix is: " `T.append` (T.pack $ show result)
    debug $ "result points after applying calibration: " `T.append` (T.pack $ show rspoints)
    return result
    where
      mClosestTransform = findClosestRotateMatrix corners npoints
      npoints = P.map (normalizePoint g) points
      corners = [ Point2 0.125 0.125, Point2 0.875 0.125
                , Point2 0.125 0.875, Point2 0.875 0.875
                ]
      

printConfig
  :: ( Monad m
     , WritesToHandle m
     , Profiled m
     )
  => Text
  -> Matrix NFloat
  -> m ()
printConfig name matrix = profile "printConfig" $ C.mapM_ putStrLn
  [ "Section \"InputClass\""
  , "\tIdentifier\t \"calibration\""
  , "\tMatchProduct\t \""
    `T.append` name `T.append` "\""
  , "\tOption\t \"TransformationMatrix\"  \"" 
    `T.append` matrixToXinputProp  matrix `T.append` "\""
  , "EndSection"
  ]

getDeviceName
  :: ( Monad m
     , ReadsEnvironment m
     , MonadThrow m
     , Debugged m
     , Profiled m
     )
  => m Text
getDeviceName = profile "getDeviceName" $ do
    mval <- lookupEnv "XICTM_DEVICE"
    case mval of
      Nothing-> throwM EDeviceNameNotFound
      Just some -> do
        debug $ "device name is " `T.append` some
        return some

