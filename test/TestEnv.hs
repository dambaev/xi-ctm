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
           , wProcess:: Map (Text,[Text]) (ExitCode, Text, Text)
           }

defWorld = World
  { wStdin = T.empty
  , wProcess = M.empty
  }

setStdin:: Text-> World-> World
setStdin sin w = w{ wStdin = sin}

addProc:: (Text,[Text],ExitCode,Text,Text)-> World-> World
addProc (cmd,args,code,sout,serr) w@World{wProcess = procMap} = w
  { wProcess = M.insert (cmd,args) (code,sout,serr) procMap
  }

type TestEnv = ReaderT World IO

instance ReadsStdin TestEnv where
    getContents = asks wStdin

instance Debugged TestEnv where
    debug _ = return ()

instance RunsProcess TestEnv where
    readProcessWithExitCode cmd args sin = do
      procMap <- asks wProcess
      return $ case M.lookup (cmd,args) procMap of
        Nothing-> (ExitFailure 1, T.empty, T.empty)
        Just some-> some
        
runTestEnv:: World-> TestEnv a-> IO a
runTestEnv world actions = runReaderT actions world

