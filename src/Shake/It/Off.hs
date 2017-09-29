{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UnicodeSyntax         #-}

module Shake.It.Off
  ( shake
  , phony, obj
  , (◉)
  , (#>), (@>), (##>), (@@>)
  , (♯), (♯♯)
  , (∫), (∰)
  , module Shake
  , module Control.Eternal
  ) where

import           Data.IORef

import           Control.Eternal
import           Control.Monad

import           Shake.It.Optional
import           Shake.It.Core       as Shake

import           Shake.It.C          as Shake
import           Shake.It.FileSystem as Shake
import           Shake.It.Haskell    as Shake
import           Shake.It.Rust       as Shake
import           Shake.It.Utils      as Shake
import           Shake.It.Version    as Shake

shake ∷ IO () → IO ()
shake maybeAction = do
  args ← getArgs
  writeIORef phonyArgs args
  maybeAction
  if | "-h" ∈ args ∨ "--help" ∈ args → displayHelp
     | otherwise → do
        myObjects ← readIORef objects
        forM_ myObjects $ uncurry compileObj

displayHelp ∷ IO ()
displayHelp = do
  myPhonyActions ← readIORef phonyActions
  forM_ (reverse myPhonyActions) $ \(r, _, d) →
    putStrLn $ "  " ++ r ++ " : " ++ d

phony :: (Optional1 [String] (String → IO () → IO ()) r) ⇒ r
phony = opt1 gPhony []

gPhony ∷ [String] → String → IO () → IO ()
gPhony [] arg phonyAction = do
  args ← readIORef phonyArgs
  if arg ∈ args
    then do phonyAction
            filtered ← removePhonyArg args arg
            when (null filtered) exitSuccess
    else do currentPhony ← readIORef phonyActions
            let new = (arg, phonyAction, "TODO") : currentPhony
            writeIORef phonyActions new
gPhony deps arg complexPhonyAction = do
  myPhonyArgs ← readIORef phonyArgs
  myPhonyActions ← readIORef phonyActions
  if arg ∈ myPhonyArgs
    then do
      myObjects ← readIORef objects
      forM_ deps $ \dep → do
        forM_ myObjects $ \(file, buildAction) →
          when (dep == file) $
            compileObj file buildAction
        forM_ myPhonyActions $ \(rule, phonyAction, _) →
          when (dep == rule) $ compilePhony rule phonyAction
      complexPhonyAction
      filtered ← removePhonyArg myPhonyArgs arg
      when (null filtered) exitSuccess
    else let new = (arg, complexPhonyAction, "TODO") : myPhonyActions
         in writeIORef phonyActions new

obj :: (Optional1 [String] (FilePath → IO () → IO ()) r) ⇒ r
obj = opt1 gObj []

gObj ∷ [String] → FilePath → IO () → IO ()
gObj [] arg buildAction = do
  currentObjects ← readIORef objects
  currentObjectList ← readIORef objectsList
  let new = (arg, buildAction) : currentObjects
  writeIORef objectsList (arg : currentObjectList)
  writeIORef objects new
gObj deps arg complexBuildAction = do
  myPhonyActions ← readIORef phonyActions
  myObjects      ← readIORef objects
  myObjectList   ← readIORef objectsList
  forM_ deps $ \dep → do
    forM_ myObjects $ \(file, buildAction) →
      when (dep == file) $
        compileObj file buildAction
    forM_ myPhonyActions $ \(rule, phonyAction, _) →
      when (dep == rule) $ compilePhony rule phonyAction
  let new = (arg, complexBuildAction) : myObjects
  writeIORef objectsList (arg : myObjectList)
  writeIORef objects new

-- operators
infixl 5 ◉
-- even >>= will have 1 priority and $ have priority 0
-- to check priority: type ":i >>=" into ghci
infixl 0 ∰, ∫, #>, ##>, @>, @@>, ♯, ♯♯

-- tuple maker
(◉) ∷ String → [String] → (String, [String])
s ◉ ss = (s, ss)

-- Phony operator
(@>) ∷ String → IO () → IO ()
r @> a = phony r a

-- Phony' operator
(@@>) ∷ (String, [String]) → IO () → IO ()
(r, d) @@> a = phony d r a

-- Unicode variant of phony
(∫) ∷ String → IO () → IO ()
r ∫ a = phony r a

-- Unicode variant of phony'
(∰) ∷ (String, [String]) → IO () → IO ()
(r, d) ∰ a = phony d r a

-- Obj operator
(#>) ∷ String → IO () → IO ()
r #> a = obj r a

-- Obj' operator
(##>) ∷ (String, [String]) → IO () → IO ()
(r, d) ##> a = obj d r a

-- Unicode Obj operator
(♯) ∷ FilePath → IO () → IO ()
r ♯ a = obj r a

-- Unicode Obj' operator
(♯♯) ∷ (FilePath, [String]) → IO () → IO ()
(r, d) ♯♯ a = obj d r a
