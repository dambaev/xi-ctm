{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Interface where

import qualified Prelude as P
import Prelude
  ( Monad
  , Maybe
  , (.)
  , ($)
  )
import Data.Int
import Control.Monad.Trans
import Data.Text
import System.Exit
import System.Process( CreateProcess)

class (Monad m) => ReadsEnvironment m where
  lookupEnv:: Text-> m (Maybe Text)

  default lookupEnv:: (MonadTrans t, ReadsEnvironment m1, m ~ t m1)
    => Text-> m (Maybe Text)
  lookupEnv key = lift $ lookupEnv key


class (Monad m) => RunsProcess m where
    readCreateProcessWithExitCode:: Text -> Text-> m (ExitCode, Text, Text)

    default readCreateProcessWithExitCode:: (MonadTrans t, RunsProcess m1, m ~ t m1)
      => Text-> Text-> m (ExitCode, Text, Text)
    readCreateProcessWithExitCode p input = 
      lift $ readCreateProcessWithExitCode p input


class (Monad m) => Debugged m where
    debug:: Text-> m ()

    default debug:: (MonadTrans t, Debugged m1, m ~ t m1)
      => Text-> m ()
    debug str = lift $ debug str

class (Monad m) => ReadsStdin m where
    getContents:: m Text

    default getContents:: (MonadTrans t, ReadsStdin m1, m ~ t m1)
      => m Text
    getContents = lift getContents
    
class (Monad m) => WritesToHandle m where
    putStrLn:: Text-> m ()

    default putStrLn :: (MonadTrans t, WritesToHandle m1, m ~ t m1)
      => Text-> m ()
    putStrLn str = lift $ putStrLn str

class (Monad m) => Profiled m where
    profile:: Text-> m a-> m a

class (Monad m) => ClockReader m where
    getMonotonicClock :: m (Int64, Int64)

    default getMonotonicClock:: (MonadTrans t, ClockReader m1, m ~t m1)
      => m (Int64, Int64)
    getMonotonicClock = lift getMonotonicClock
