{-# LANGUAGE
    CPP
  , MultiWayIf
  , LambdaCase
  , UnicodeSyntax
  , RankNTypes
  #-}

module Shake.It.Off
  ( shake
  , phony
  , module Shake
  ) where

import System.Process
import System.Environment
import System.Exit
import System.IO.Unsafe

import Data.IORef

import Control.Monad
import Control.Eternal

import Shake.It.Core as Shake
import Shake.It.Version as Shake
import Shake.It.Haskell as Shake

phonyArgs :: IORef [String]
{-# NOINLINE phonyArgs #-}
phonyArgs = unsafePerformIO (newIORef [])

shake :: IO () → IO ()
shake action = do
  getArgs >>= writeIORef phonyArgs
  action

removePhonyArg :: [String] → String → IO [String]
removePhonyArg args arg = do
  let filtered = filter (/= arg) args
  writeIORef phonyArgs filtered
  return filtered

phony :: String → IO () → IO ()
phony arg phonyAction = do
  args ← readIORef phonyArgs
  when (arg ∈ args) $ do
    phonyAction
    filtered ← removePhonyArg args arg
    when (null filtered) exitSuccess
