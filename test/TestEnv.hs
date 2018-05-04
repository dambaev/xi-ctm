{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module TestEnv where

import Control.Monad.Reader
import Data.Text as T
import System.Exit
import Data.Map as M
import Interface
import Control.Monad.Catch as E

data World = World
           { wStdin:: Text
           , wProcess:: Map Text (ExitCode, Text, Text)
           }

defWorld = World
  { wStdin = T.empty
  , wProcess = M.empty
  }

setStdin:: Text-> World-> World
setStdin sin w = w{ wStdin = sin}

addProc:: (Text,ExitCode,Text,Text)-> World-> World
addProc (cmd,code,sout,serr) w@World{wProcess = procMap} = w
  { wProcess = M.insert cmd (code,sout,serr) procMap
  }

type TestEnv m = ReaderT World m

instance Monad m => ReadsStdin (TestEnv m) where
    getContents = asks wStdin

instance Monad m => Debugged (TestEnv m) where
    debug _ = return ()

instance (Monad m) => RunsProcess (TestEnv m) where
    readCreateProcessWithExitCode cmd sin = do
      procMap <- asks wProcess
      return $ case M.lookup cmd procMap of
        Nothing-> (ExitFailure 1, T.empty, T.empty)
        Just some-> some

instance Monad m => Profiled (TestEnv m) where
    profile _ action = action
        
runTestEnv:: World-> TestEnv m a-> m a
runTestEnv world actions = runReaderT actions world

