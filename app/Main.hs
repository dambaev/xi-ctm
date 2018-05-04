{-# LANGUAGE FlexibleInstances #-}
module Main where

import CTM
import Performance
import DebugEnv
import Interface
import System.Environment as IO
import Control.Monad.Trans
import Control.Monad.Catch

data RunningMode
    = Performance
    | Profile
    | Verbose
    | Debug

instance RunsProcess (DebugEnvT (PerformanceEnvT IO ))
instance MonadThrow (DebugEnvT (PerformanceEnvT IO)) where
    throwM e = lift $ lift $ throwM e
instance ReadsStdin (DebugEnvT (PerformanceEnvT IO))
instance ReadsEnvironment (DebugEnvT (PerformanceEnvT IO))

main :: IO ()
main = do
    mode <- getRunningMode
    case mode of
      Performance -> runPerformance realMain
      Debug -> runPerformance (runDebugEnv realMain)
{-                      Profile -> runProfile
                      Verbose -> runVerbose
                      Debug -> runDebug-}
    
getRunningMode:: IO RunningMode
getRunningMode = do
    mmode <- IO.lookupEnv "XICTM_MODE"
    return $ case mmode of
      Just "profile" -> Profile
      Just "verbose" -> Verbose
      Just "debug" -> Debug
      _       -> Performance



