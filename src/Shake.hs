{-# LANGUAGE CPP           #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnicodeSyntax #-}

module Shake
  ( shakeIt
  , shakeItOff
  , module Shake.It.Off
  ) where

import           Script
import           Shake.It.Off

import           System.IO

import           Data.String.Utils

import           Control.Concurrent
import           Control.Eternal
import           Control.Exception
import           Control.Monad

shakeIt ∷ [String]
        → String  -- current directory
        → Bool    -- force
        → Bool    -- pretend
        → String  -- platform
        → IO ()
shakeIt args current force pretend platform = do
  let fullNamelhs = current </> "shake.it.lhs"
      fullNamehs  = current </> "shake.it.hs"
      shakeShake  = shakeItOff args current force pretend platform
  existslhs ← doesFileExist fullNamelhs
  existshs  ← doesFileExist fullNamehs
  if | existslhs → shakeShake fullNamelhs
     | existshs  → shakeShake fullNamehs
     | otherwise →
        unless ("-h" ∈ args ∨ "--help" ∈ args) $
          putStrLn "no shake.it.hs / shake.it.lhs file"

shakeItOff ∷ [String]
           → String   -- current directory
           → Bool     -- force
           → Bool     -- pretend
           → String   -- platform
           → String   -- shake file
           → IO ()
shakeItOff args dir force pretend platform shakefile = do
  let cscr = if | platform ∈ ["Win_x64", "Win"] → "shake.it.off.exe"
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
                   >>= \case ExitFailure ε → do
                               hPrint stderr ε
                               exitFailure
                             ExitSuccess → return ()

  let ifForce =
        if | force → filter (\ο → ο /= "-f"
                               && ο /= "--force") args
           | otherwise → args
      shArgs = filter (\ο → not (startswith "-p" ο
                              || startswith "--platform" ο)) ifForce

  unless pretend $
    runShake cscr shArgs
