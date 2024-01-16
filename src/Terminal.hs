module Terminal where

import GHC.IO.Handle (hSetBuffering, BufferMode (NoBuffering))
import System.IO (stdout)

noBuffering :: IO ()
noBuffering = hSetBuffering stdout NoBuffering
