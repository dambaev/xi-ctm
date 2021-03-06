{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Performance where

import Types
import Interface

import System.Environment as IO
import Data.Text as T
import Prelude as P
import System.IO as IO
import Control.Monad.IO.Class
import Control.Monad.Catch as E
import Control.Monad.Trans
import System.Clock as C
import qualified Data.Text.IO as T
import System.Process as IO

newtype Monad m => PerformanceEnvT m a = PE
                  { runPE:: m a
                  }
        deriving
          ( Monad
          , Applicative
          , Functor
          , MonadIO
          )

runPerformance
  :: Monad m
  => PerformanceEnvT m a
  -> m a
runPerformance actions = runPE actions

instance MonadTrans PerformanceEnvT where
    lift actions = PE $ actions

instance (MonadIO m, MonadThrow m) => MonadThrow (PerformanceEnvT m) where
    throwM e = lift $ throwM e

instance MonadIO m => ReadsEnvironment (PerformanceEnvT m) where
    lookupEnv key = wrap <$> ( liftIO $ IO.lookupEnv $ T.unpack key) 
      where
        wrap Nothing = Nothing
        wrap (Just str) = Just $ T.pack str

instance MonadIO m => Debugged (PerformanceEnvT m) where
    debug _ = return ()

instance MonadIO m => ReadsStdin (PerformanceEnvT m) where
    getContents = liftIO T.getContents

instance MonadIO m => RunsProcess (PerformanceEnvT m) where
    readCreateProcessWithExitCode cmd input = do
      (code, outS, errS) <- liftIO $ IO.readCreateProcessWithExitCode 
        (shell $ T.unpack cmd)
        (T.unpack input)
      return (code, T.pack outS, T.pack errS)

instance MonadIO m => WritesToHandle (PerformanceEnvT m) where
    putStrLn = liftIO . T.putStrLn

instance MonadIO m => ClockReader (PerformanceEnvT m) where
    getMonotonicClock = do
      TimeSpec sec nsec <- liftIO $ getTime Boottime
      return (sec,nsec)

instance MonadIO m => Profiled (PerformanceEnvT m) where
    profile _ action = action
