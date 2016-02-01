{-# LANGUAGE
    UnicodeSyntax
  #-}

module Shake.It.Version
  ( printVersion
  ) where

import qualified Paths_ShakeItOff as My
import Data.Version (showVersion)

printVersion :: IO ()
printVersion = putStrLn $ showVersion My.version
