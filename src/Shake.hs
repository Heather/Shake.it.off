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
  ) where

import Script

import System.Info (os)
import System.Exit
import System.Directory
import System.IO
import System.FilePath ((</>))
import System.Process

import Control.Monad
import Control.Exception
import Control.Eternal
import Control.Concurrent

shakeIt :: [String] → String → IO ()
shakeIt args current = do
  let fullNamelhs = current </> "shake.it.lhs"
      fullNamehs  = current </> "shake.it.hs"
      shakeShake  = shakeItOff args current
  existslhs ← doesFileExist fullNamelhs
  if existslhs
    then shakeShake fullNamelhs
    else do
      existshs ← doesFileExist fullNamehs
      if existshs then shakeShake fullNamehs
                  else putStrLn "no shake.it.hs / shake.it.lhs file"

shakeItOff :: [String] → String → String → IO ()
shakeItOff args dir shakefile = do
  let cscr = if | os ∈ ["win32", "mingw32", "cygwin32"] → "shake.it.off.exe"
                | otherwise → "shake.it.off"

  cscrExists  ← doesFileExist cscr
  doRecompile ← if cscrExists
                  then do
                    scrMTime  ← getMTime shakefile
                    cscrMTime ← getMTime cscr
                    return $ cscrMTime <= scrMTime
                  else return True

  when doRecompile $ system ("ghc --make -o " ++ cscr ++ " " ++ shakefile)
                   >>= \case ExitFailure i → do
                               hPrint stderr i
                               exitFailure
                             ExitSuccess → return ()

  runShake cscr args
