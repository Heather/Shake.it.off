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
  , (</>) -- Filepath
  , (◉)
  , (#>), (@>), (##>), (@@>)
  , (♯), (♯♯)
  , (∫), (∰)
  , module Shake
  ) where

import System.Process
import System.Environment
import System.Exit
import System.Directory
import System.FilePath ((</>))

import Data.IORef

import Control.Monad
import Control.Eternal

import Shake.It.Core as Shake
import Shake.It.Version as Shake
import Shake.It.C as Shake
import Shake.It.Haskell as Shake

shake :: IO () → IO ()
shake maybeAction = do
  getArgs >>= writeIORef phonyArgs
  maybeAction
  myObjects ← readIORef objects
  forM_ myObjects $ uncurry compileObj

phony :: String → IO () → IO ()
phony arg phonyAction = do
  args ← readIORef phonyArgs
  if arg ∈ args
    then do phonyAction
            filtered ← removePhonyArg args arg
            when (null filtered) exitSuccess
    else do currentPhony ← readIORef phonyActions
            let new = (arg, phonyAction) : currentPhony
            writeIORef phonyActions new

phony' :: (String, [String]) → IO () → IO ()
phony' (arg, deps) complexPhonyAction = do
  myPhonyArgs ← readIORef phonyArgs
  myPhonyActions ← readIORef phonyActions
  if arg ∈ myPhonyArgs
    then do
      myObjects ← readIORef objects
      forM_ deps $ \dep → do
        forM_ myObjects $ \(file, buildAction) →
          when (dep == file) $
            compileObj file buildAction
        forM_ myPhonyActions $ \(rule, phonyAction) →
          when (dep == rule) $ compilePhony rule phonyAction
      complexPhonyAction
      filtered ← removePhonyArg myPhonyArgs arg
      when (null filtered) exitSuccess
    else let new = (arg, complexPhonyAction) : myPhonyActions
         in writeIORef phonyActions new

obj :: FilePath → IO () → IO ()
obj arg buildAction = do
  currentObjects ← readIORef objects
  currentObjectList ← readIORef objectsList
  let new = (arg, buildAction) : currentObjects
  writeIORef objectsList (arg : currentObjectList)
  writeIORef objects new

obj' :: (FilePath, [String]) → IO () → IO ()
obj' (arg, deps) complexBuildAction = do
  myPhonyArgs    ← readIORef phonyArgs
  myPhonyActions ← readIORef phonyActions
  myObjects      ← readIORef objects
  myObjectList   ← readIORef objectsList
  forM_ deps $ \dep → do
    forM_ myObjects $ \(file, buildAction) →
      when (dep == file) $
        compileObj file buildAction
    forM_ myPhonyActions $ \(rule, phonyAction) →
      when (dep == rule) $ compilePhony rule phonyAction
  let new = (arg, complexBuildAction) : myObjects
  writeIORef objectsList (arg : myObjectList)
  writeIORef objects new

-- operators
infixl 2 ∰, ◉, ∫, #>, ##>, @>, @@>, ♯, ♯♯

-- tuple maker
(◉) :: String → [String] → (String, [String])
s ◉ ss = (s, ss)

-- Phony operator
(@>) :: String → IO () → IO ()
r @> a = phony r a

-- Phony' operator
(@@>) :: (String, [String]) → IO () → IO ()
r @@> a = phony' r a

-- Unicode variant of phony
(∫) :: String → IO () → IO ()
r ∫ a = phony r a

-- Unicode variant of phony'
(∰) :: (String, [String]) → IO () → IO ()
r ∰ a = phony' r a

-- Obj operator
(#>) :: String → IO () → IO ()
r #> a = obj r a

-- Obj' operator
(##>) :: (String, [String]) → IO () → IO ()
r ##> a = obj' r a

-- Unicode Obj operator
(♯) :: FilePath → IO () → IO ()
r ♯ a = obj r a

-- Unicode Obj' operator
(♯♯) :: (FilePath, [String]) → IO () → IO ()
r ♯♯ a = obj' r a
