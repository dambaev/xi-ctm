{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ProfileEnv where

import Control.Monad.Trans
import Interface
import Data.Text as T
import qualified Prelude as P
import Prelude
  ( Monad
  , Applicative
  , Functor
  , ($)
  , return
  , show
  )

newtype ProfileEnvT m a = PE
    { runPE:: m a
    }
    deriving
      ( Monad
      , Applicative
      , Functor
      , WritesToHandle
      , ClockReader
      )

runProfileEnv:: ProfileEnvT m a-> m a
runProfileEnv = runPE 

instance MonadTrans ProfileEnvT where
    lift = PE

instance Monad m => Debugged (ProfileEnvT m) where
    debug _ = return ()

instance (Monad m, WritesToHandle m, ClockReader m) => Profiled (ProfileEnvT m) where
    profile mark action = do
      (sec,nsec) <- getMonotonicClock
      putStrLn $ "[" `T.append` T.pack ( show sec) 
                  `T.append` "." `T.append` T.pack ( show nsec)
                  `T.append` "] > start: " `T.append` mark
      ret <- action
      (sec,nsec) <- getMonotonicClock
      putStrLn $ "[" `T.append` T.pack ( show sec) 
                  `T.append` "." `T.append` T.pack ( show nsec)
                  `T.append` "] < end: " `T.append` mark
      return ret

