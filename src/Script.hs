{-# LANGUAGE CPP            #-}
{-# LANGUAGE MultiWayIf     #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE UnicodeSyntax  #-}
{-# LANGUAGE RankNTypes     #-}

module Script
  ( getMTime
  , runShake
  ) where

import           System.Directory (getModificationTime)
import           System.Process
import           System.Exit
import           System.IO

import           Data.Time.Clock
import           Data.List

#if ! ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
import           System.Posix.Process
import           System.Posix.Files
import           System.Posix.Types
#endif

import           Control.Monad

#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
getMTime :: FilePath → IO UTCTime
getMTime f = getModificationTime f
#else
getMTime :: FilePath → IO EpochTime
getMTime f = fmap modificationTime (getFileStatus f)
#endif

runShake :: String → [String] → IO ()
runShake cscr args =
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  do pid ← runCommand (cscr ++ " " ++ intercalate " " args)
     waitForProcess pid >>= exitWith
#else
  executeFile cscr False args Nothing
#endif
