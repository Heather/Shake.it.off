{-# LANGUAGE
    CPP
  , MultiWayIf
  , LambdaCase
  , UnicodeSyntax
  , RankNTypes
  #-}

module Script
  ( getMTime
  , shakeItOff
  ) where

import System.Directory (getModificationTime)
import System.Process
import System.Exit
import System.IO

import Data.Time.Clock

#if ! ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
import System.Posix.Process
import System.Posix.Files
#endif

import Control.Monad

getMTime :: FilePath → IO UTCTime
getMTime =
#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
  getModificationTime
#else
  liftM modificationTime (getFileStatus f)
#endif

shakeItOff :: String → IO ()
shakeItOff cscr =
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  do pid ← runCommand cscr
     waitForProcess pid >>= exitWith
#else
  executeFile cscr False args Nothing
#endif
