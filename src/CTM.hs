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

realMain
  :: ( Monad m
     , RunsProcess m
     , MonadThrow m
     , Debugged m
     , ReadsStdin m
     , ReadsEnvironment m
     , WritesToHandle m
     )
  => m ()
realMain = do
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
     )
  => m Geometry
getDisplayGeometry = do
    debug "running xrandr"
    (code, out, err) <- readProcessWithExitCode "xrandr" [] ""
    when (code /= ExitSuccess) $ throwM EXrandrFailed
    extractDisplayGeometry $ lines out

extractDisplayGeometry
  :: ( Monad m
     , Debugged m
     )
  => [Text]
  -> m Geometry
extractDisplayGeometry lined = do
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
     )
  => [Text]
  -> m [Point2]
extractPoints lined = do
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
     )
  => Text
  -> Matrix Float
  -> m ()
applyCTM name matrix = do
    debug $ "running " `T.append` cmd
    (code,out,err) <- readCreateProcessWithExitCode (shell $ T.unpack cmd)
                      ""
    when (code /= ExitSuccess) $ throwM EXInputFailed
    where
      cmd = "xinput set-prop \"" 
        `T.append` name 
        `T.append` "\" 'Coordinate Transformation Matrix' " 
        `T.append` matrixToXinputProp matrix


matrixToXinputProp
  :: Matrix Float
  -> Text
matrixToXinputProp (M _ _ els) = (T.pack . P.intercalate " " . P.map (P.intercalate " " . P.map show . V.toList) . V.toList) els


normal = M.fromList
  [ [ 1.0, 0.0, 0.0]
  , [ 0.0, 1.0, 0.0]
  , [ 0.0, 0.0, 1.0]
  ]

swapped :: Matrix Float
swapped = M.fromList
  [ [ 0.0, 1.0, 0.0]
  , [ 1.0, 0.0, 0.0]
  , [ 0.0, 0.0, 1.0]
  ]

leftRotate :: Matrix Float
leftRotate = M.fromList
  [ [ 0.0,-1.0, 1.0]
  , [ 1.0, 0.0, 0.0]
  , [ 0.0, 0.0, 1.0]
  ]

rightRotate:: Matrix Float
rightRotate = M.fromList
  [ [ 0.0, 1.0, 0.0]
  , [-1.0, 0.0, 1.0]
  , [ 0.0, 0.0, 1.0]
  ]

invert:: Matrix Float
invert = M.fromList
  [ [-1.0, 0.0, 1.0]
  , [ 0.0,-1.0, 1.0]
  , [ 0.0, 0.0, 1.0]
  ]


matrixes:: [(Text, Matrix Float)]
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
    ]

isCorrectOrientation
  :: Geometry
  -> [Point2]
  -> Bool
isCorrectOrientation g@(Geometry w h) points@(p0:p1:p2:p3:_) = 
    P.all helper cornersPointsLengths
    where
      topLeft = Point2 0 0
      topRight = Point2 w 0
      bottomLeft = Point2 0 h
      bottomRight = Point2 w h
      corners = [topLeft, topRight, bottomLeft, bottomRight]
      cornersPoints = P.zipWith (,) corners points
      cornersPointsLengths = P.map (\(l,r)-> (l,r,V.lengthP l r)) cornersPoints
      helper (corner,point,len) = 
        P.all (\rpoint-> len < V.lengthP corner rpoint) restPoints
        where
          restPoints = P.filter (/=point) points

getCornersDiffs
  :: Geometry
  -> [Point2]
  -> [Float]
getCornersDiffs g@(Geometry w h) points = P.map (\(l,r)-> V.lengthP l r) $  P.zipWith (,) corners points
    where
      topLeft = Point2 0 0
      topRight = Point2 w 0
      bottomLeft = Point2 0 h
      bottomRight = Point2 w h
      corners = [ topLeft, topRight, bottomLeft, bottomRight ]

findClosestTransform
  :: Geometry
  -> [Point2]
  -> Maybe ( [Text]
           , Matrix Float
           )
findClosestTransform g@(Geometry w h) points = 
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
        cdiff = getCornersDiffs g ctpoints
        diff = getCornersDiffs g tpoints
        ctpoints = P.map (translatePoint g cmat) points
        tpoints = P.map (translatePoint g mat) points
    check names matrix = findTheBestOption correctOrientation
      where
        options = P.map (\(name,mat)-> (name:names, matProduct matrix mat)) matrixes
        correctOrientation = P.filter (\( _, mat)-> isCorrectOrientation g (P.map (translatePoint g mat) points)) options

translatePoint
  :: Geometry
  -> Matrix Float
  -> Point2
  -> Point2
translatePoint g@(Geometry w h) m p = scalePoint g tnpoint
  where
    (Point2 nx ny) = normalizePoint g p
    pointM = M.fromList [[nx], [ny], [1]]
    tnpoint = matToPoint $ matProduct m pointM

matToPoint
  :: Matrix Float
  -> Point2
matToPoint (M r c els)
  | r /= 3 || c /= 1 = error "matToPoint only can be applied to 3x1"
  | otherwise = Point2 x y
    where
      x = V.head $ els V.! 0
      y = V.head $ els V.! 1

normalizePoint
  :: Geometry
  -> Point2
  -> Point2
normalizePoint (Geometry w h) (Point2 x y) = Point2 (x/w) (y/h)

scalePoint
  :: Geometry
  -> Point2
  -> Point2
scalePoint (Geometry w h) (Point2 x y) = Point2 (x*w) (y*h)

calibrateMatrix
  :: Geometry
  -> [ Point2]
  -> Matrix Float
calibrateMatrix g@(Geometry w h) points = M.fromList
  [ [c0, 0, c1]
  , [ 0,c2, c3]
  , [ 0, 0, 1]
  ]
    where
      ((Point2 x0' y0'):(Point2 x1' y1'):(Point2 x2' y2'):(Point2 x3' y3'):_) = points
      touchAreaWidth | diff_x > 0.2 = x1 - x0
                     | otherwise = w
      touchAreaHeight | diff_y > 0.2 = y1 - y0
                      | otherwise = h
      c0 | touchAreaWidth < w = 1 + touchAreaWidth / w
         | touchAreaWidth == w = 1
         | otherwise = 1 - touchAreaWidth / w
      c1 | x == 0 = 0
         | touchAreaWidth > w = x / w
         | otherwise = 1 + x / w
      c2 | touchAreaHeight < h = 1 + touchAreaHeight / h
         | touchAreaHeight == h = 1
         | otherwise = 1 - touchAreaHeight / h
      c3 | y == 0 = 0
         | touchAreaHeight > h = y / h
         | otherwise = 1 + y / h
      block_w = w / 8
      block_h = h / 8
      x0 = min x0' x2'
      x1 = max x1' x3'
      y0 = min y0' y1'
      y1 = max y2' y3'
      diff_x = (max block_w x0 - min block_w x0) / block_w
      diff_y = (max block_h y0 - min block_h y0) / block_h
      x | diff_x > 0.2 = x0
        | otherwise = 0
      y | diff_y > 0.2 = y0
        | otherwise = 0


guessCoordinateMatrixTransform
  :: ( Monad m
     , Debugged m
     , MonadThrow m
     )
  => Geometry
  -> [Point2]
  -> m (Matrix Float)
guessCoordinateMatrixTransform g@(Geometry w h) points@(p0:p1:p2:p3:_) = do
    when ( mClosestTransform == Nothing) $ 
      throwM ENoTransformMatrixFound
    let Just (matrixNames,transformM) = mClosestTransform
        tpoints = P.map (translatePoint g transformM) points
        calibrateM = calibrateMatrix g tpoints
        result' = matProduct calibrateM transformM
        result = matSetEl 1 2 (matGetEl 1 2 calibrateM) 
          $ matSetEl 0 2 (matGetEl 0 2 calibrateM) result'
    debug $ "those transformations had been applied " `T.append` (T.pack $ show matrixNames)
    debug $ "translated points are: " `T.append` (T.pack $ show tpoints)
    debug $ "calibrate matrix is: " `T.append` (T.pack $ show calibrateM)
    debug $ "result matrix is: " `T.append` (T.pack $ show result)
    return result
    where
      mClosestTransform = findClosestTransform g points
      

printConfig
  :: ( Monad m
     , WritesToHandle m
     )
  => Text
  -> Matrix Float
  -> m ()
printConfig name matrix = C.mapM_ putStrLn
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
     )
  => m Text
getDeviceName = do
    mval <- lookupEnv "XICTM_DEVICE"
    case mval of
      Nothing-> throwM EDeviceNameNotFound
      Just some -> do
        debug $ "device name is " `T.append` some
        return some
