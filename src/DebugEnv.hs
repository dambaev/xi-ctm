{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DebugEnv where

import qualified Prelude as P
import Control.Monad.Trans
import Prelude
  ( Monad
  , Applicative
  , Functor
  )
import Interface

newtype Monad m => DebugEnvT m a = DE
        { runDE:: m a
        }
  deriving
    ( Monad
    , Applicative
    , Functor
    , WritesToHandle
    )

runDebugEnv
  :: Monad m
  => DebugEnvT m a
  -> m a
runDebugEnv actions = runDE actions

instance (Monad m, WritesToHandle m) => Debugged (DebugEnvT m) where
    debug = putStrLn

instance MonadTrans DebugEnvT where
    lift = DE 
