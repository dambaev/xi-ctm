module Main where

import CTM
import Performance
import System.Environment as IO

data RunningMode
    = Performance
    | Profile
    | Verbose
    | Debug

main :: IO ()
main = do
    mode <- getRunningMode
    let runHelper = case mode of
                      Performance -> runPerformance
{-                      Profile -> runProfile
                      Verbose -> runVerbose
                      Debug -> runDebug-}
    runHelper realMain
    
getRunningMode:: IO RunningMode
getRunningMode = do
    mmode <- IO.lookupEnv "XICTM_MODE"
    return $ case mmode of
      Just "profile" -> Profile
      Just "verbose" -> Verbose
      Just "debug" -> Debug
      _       -> Performance



