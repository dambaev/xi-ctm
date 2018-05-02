module Performance where

import Types
import Interface

import System.Environment as IO
import Data.Text as T
import Prelude as P
import System.IO as IO
import qualified Data.Text.IO as T
import System.Process as IO

instance ReadsEnvironment IO where
    lookupEnv key = wrap <$> ( IO.lookupEnv $ T.unpack key) 
      where
        wrap Nothing = Nothing
        wrap (Just str) = Just $ T.pack str

instance Debugged IO where
    debug _ = return ()

instance ReadsStdin IO where
    getContents = T.getContents

instance RunsProcess IO where
    readProcessWithExitCode cmd args input = do
      (code, outS, errS) <- IO.readProcessWithExitCode 
        (T.unpack cmd)
        (P.map T.unpack args)
        (T.unpack input)
      return (code, T.pack outS, T.pack errS)

runPerformance = id

instance WritesToHandle IO where
    putStrLn = T.putStrLn
