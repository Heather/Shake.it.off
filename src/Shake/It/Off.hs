{-# LANGUAGE
    CPP
  , MultiWayIf
  , LambdaCase
  , UnicodeSyntax
  , RankNTypes
  , DataKinds
  , OverloadedStrings
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  #-}

module Shake.It.Off
  ( shake
  , phony, obj
  , (#>), (@>), (∰), (∫)
  , module Shake
  -- , module Data.Optional
  ) where

import System.Process
import System.Environment
import System.Exit
import System.IO.Unsafe
import System.Directory
import System.FilePath ((</>))

import Data.IORef

import Control.Monad
import Control.Eternal

import Shake.It.Core as Shake
import Shake.It.Version as Shake
import Shake.It.Haskell as Shake

phonyArgs :: IORef [String]
{-# NOINLINE phonyArgs #-}
phonyArgs = unsafePerformIO (newIORef [])

objects :: IORef [(String, IO ())]
{-# NOINLINE objects #-}
objects = unsafePerformIO (newIORef [])

shake :: IO () → IO ()
shake maybeAction = do
  getArgs >>= writeIORef phonyArgs
  maybeAction
  currentDir ← getCurrentDirectory
  myObjects  ← readIORef objects
  forM_ myObjects $ \(file, buildAction) → do
    let fullPath = currentDir </> file
    buildAction -- building this file
    objExists ← doesFileExist fullPath
    unless objExists exitFailure

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

obj :: FilePath → IO () → IO ()
obj arg buildAction = do
  currentObjects ← readIORef objects
  let new = (arg, buildAction) : currentObjects
  writeIORef objects new

infixl 2 ∰, ∫, #>, @>

-- Phony operator
(#>) :: String → IO () → IO ()
r #> a = phony r a

-- Unicode variant of phony
(∰) :: String → IO () → IO ()
r ∰ a = phony r a

-- Obj operator
(@>) :: String → IO () → IO ()
r @> a = obj r a

-- Unicode Obj operator
(∫) :: String → IO () → IO ()
r ∫ a = obj r a
