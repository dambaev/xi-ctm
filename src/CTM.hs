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
  input <- getContents
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
    prefix = "default connected "
    outputFilter:: Text-> Text-> Text
    outputFilter tmp line 
      | T.null tmp && prefix `isPrefixOf` line = 
          T.takeWhile isAlphaNum $ T.drop (T.length prefix) line
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
    debug $ "extracted (mname,points) = " `T.append` (T.pack $ show points)
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
     )
  => Text
  -> Matrix Float
  -> m ()
applyCTM name matrix = do
    (code,out,err) <- readProcessWithExitCode "xinput"
                      [ "set-prop"
                      , name
                      , "Coordinate Transformation Matrix"
                      , matrixToXinputProp matrix
                      ]
                      ""
    when (code /= ExitSuccess) $ throwM EXInputFailed


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
  , [ 0.0,-1.0, 0.0]
  , [ 0.0, 0.0, 1.0]
  ]


-- | translate size m
-- translates negative coordinates (in 3x1 matrix form) to positive
-- ie: size=(800,600) m = [[-200],[-100],[1]] = [[600],[500],[1]]
translate
  :: Geometry
  -> Matrix Float
  -> Matrix Float
translate (Geometry w h) (M r c el) 
  | r /= 3 || c /= 1 = error "translate is only should be used for point coordinate matrix"
  | otherwise = newm
    where
      x = V.head $ el ! 0
      y = V.head $ el ! 1
      newx = if x < 0
               then w + x
               else x
      newy = if y < 0
               then h + y
               else y
      newm = M.fromList
        [ [newx]
        , [newy]
        , [1]
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
           , [Point2]
           )
findClosestTransform g@(Geometry w h) points = 
    case findTheBestOption totalOptions of
      [] -> Nothing
      ((names, mat, pointsM ):_) -> Just (names, mat, P.map matToPoint pointsM)
  where
    totalOptions = normalTry P.++ swappedTry
    normalTry = check ["normal"] normal points 
    swappedTry = check ["inverted"] invert swappedPoints
    swappedPoints = P.map matToPoint $ P.map (translate g . matProduct invert) $ P.map pointToMat points
    findTheBestOption options = P.foldl' helper [] options
    helper [] (name, mat, pointsM) = [(name, mat, pointsM)]
    helper same@(( cname, cmat, cpointsM):_) (name, mat, pointsM) 
      | P.all (\(l, r)-> l < r) diffs = same
      | otherwise = [(name, mat, pointsM)]
      where
        diffs = P.zipWith (,) cdiff diff
        cdiff = getCornersDiffs g cpoints
        diff = getCornersDiffs g points
        cpoints = P.map matToPoint cpointsM
        points = P.map matToPoint pointsM
    check names matrix points = findTheBestOption correctOrientation
      where
        pointsM = P.map pointToMat points
        options = P.map (\(name,mat)-> (name:names, matProduct matrix mat, P.map (matProduct mat) pointsM)) matrixes
        correctOrientation = P.filter (\( _, _, cpointsM)-> isCorrectOrientation g (P.map matToPoint cpointsM)) options


pointToMat
  :: Point2
  -> Matrix Float
pointToMat (Point2 x y) = M.fromList [[x],[y],[1]]

matToPoint
  :: Matrix Float
  -> Point2
matToPoint (M r c el) 
  | r /= 3 && c /= 1 = error "only 3x1 matrix can be converted to point2"
  | otherwise = Point2 x y
    where
      x = V.head $ el V.! 0
      y = V.head $ el V.! 1

calibrateMatrix
  :: Geometry
  -> Point2
  -> Point2
  -> Point2
  -> Matrix Float
calibrateMatrix g@(Geometry w h) 
                p0@(Point2 x0 y0) 
                p1@(Point2 x1 _)
                p2@(Point2 _ y1) = M.fromList
  [ [c0, 0, c1]
  , [ 0,c2, c3]
  , [ 0, 0, 1]
  ]
    where
      touchAreaWidth | diff_x > 0.2 = x1 - x0
                     | otherwise = w
      touchAreaHeight | diff_y > 0.2 = y1 - y0
                      | otherwise = h
      c0 = touchAreaWidth / w
      c1 = x / w
      c2 = touchAreaHeight / h
      c3 = y / h
      block_w = w / 8
      block_h = h / 8
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
    let Just (matrixNames,transformM, newpoints) = mClosestTransform
        (x0:x1:x2:x3:_) = newpoints
        calibrateM = calibrateMatrix g x0 x1 x2
        result = matProduct transformM calibrateM
    debug $ "those transformations had been applied " `T.append` (T.pack $ show matrixNames)
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
     )
  => m Text
getDeviceName = do
    mval <- lookupEnv "XICTM_DEVICE"
    case mval of
      Nothing-> throwM EDeviceNameNotFound
      Just some -> return some
