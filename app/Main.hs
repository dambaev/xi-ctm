{-# LANGUAGE FlexibleInstances #-}
module Main where

import CTM
import Performance
import DebugEnv
import ProfileEnv
import Interface
import System.Environment as IO
import Control.Monad.Trans
import Control.Monad.Catch

data RunningMode
    = Performance
    | Profile
    | Debug

instance RunsProcess (DebugEnvT (PerformanceEnvT IO ))
instance MonadThrow (DebugEnvT (PerformanceEnvT IO)) where
    throwM e = lift $ lift $ throwM e
instance ReadsStdin (DebugEnvT (PerformanceEnvT IO))
instance ReadsEnvironment (DebugEnvT (PerformanceEnvT IO))
instance RunsProcess (ProfileEnvT (PerformanceEnvT IO ))
instance MonadThrow (ProfileEnvT (PerformanceEnvT IO)) where
    throwM e = lift $ lift $ throwM e
instance ReadsStdin (ProfileEnvT (PerformanceEnvT IO))
instance ReadsEnvironment (ProfileEnvT (PerformanceEnvT IO))
instance Profiled (DebugEnvT (PerformanceEnvT IO )) where
    profile mark action = action

main:: IO ()
main = do
    mode <- getRunningMode
    case mode of
      Performance -> runPerformance realMain
      Debug -> runPerformance (runDebugEnv realMain)
      Profile -> runPerformance (runProfileEnv realMain)
    
getRunningMode:: IO RunningMode
getRunningMode = do
    mmode <- IO.lookupEnv "XICTM_MODE"
    return $ case mmode of
      Just "profile" -> Profile
      Just "debug" -> Debug
      _       -> Performance



