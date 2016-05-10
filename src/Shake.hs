{-# LANGUAGE
    CPP
  , MultiWayIf
  , LambdaCase
  , UnicodeSyntax
  , RankNTypes
  #-}

module Shake
  ( shakeIt
  , shakeItOff
  , module Shake.It.Off
  ) where

import Script
import Shake.It.Off

import System.IO

import Data.String.Utils

import Control.Monad
import Control.Exception
import Control.Eternal
import Control.Concurrent

shakeIt :: [String] → String → Bool → String → IO ()
shakeIt args current force _ = do -- _ is Platform
  let fullNamelhs = current </> "shake.it.lhs"
      fullNamehs  = current </> "shake.it.hs"
      shakeShake  = shakeItOff args current force
  existslhs ← doesFileExist fullNamelhs
  existshs  ← doesFileExist fullNamehs
  if | existslhs → shakeShake fullNamelhs
     | existshs  → shakeShake fullNamehs
     | otherwise →
        unless ("-h" ∈ args ∨ "--help" ∈ args) $
          putStrLn "no shake.it.hs / shake.it.lhs file"

shakeItOff :: [String] → String → Bool → String → IO ()
shakeItOff args dir force shakefile = do
  let cscr = if | os ∈ ["win32", "mingw32", "cygwin32"] → "shake.it.off.exe"
                | otherwise → "shake.it.off"

  cscrExists  ← doesFileExist cscr
  doRecompile ←
    if | force → return True
       | cscrExists → do
          scrMTime  ← getMTime shakefile
          cscrMTime ← getMTime cscr
          return $ cscrMTime <= scrMTime
       | otherwise → return True

  when doRecompile $ system ("ghc --make -o " ++ cscr ++ " " ++ shakefile)
                   >>= \case ExitFailure i → do
                               hPrint stderr i
                               exitFailure
                             ExitSuccess → return ()

  let ifForce =
        if | force → filter (\o → o /= "-f"
                               && o /= "--force") args
           | otherwise → args
      ifPlatform =
        if | force → filter (\o → not (startswith "-p" o
                                    || startswith "--platform" o)) ifForce
           | otherwise → ifForce

  runShake cscr ifPlatform
