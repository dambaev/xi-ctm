module Interface where

import Data.Text
import System.Exit

class (Monad m) => ReadsEnvironment m where
  lookupEnv:: Text-> m (Maybe Text)


class (Monad m) => RunsProcess m where
    readProcessWithExitCode:: Text-> [Text] -> Text-> m (ExitCode, Text, Text)

class (Monad m) => Debugged m where
    debug:: Text-> m ()

class (Monad m) => ReadsStdin m where
    getContents:: m Text
    
class (Monad m) => WritesToHandle m where
    putStrLn:: Text-> m ()
