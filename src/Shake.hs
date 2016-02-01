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
import System.Environment( getArgs )
import System.Exit
import System.Directory
import System.IO
import System.FilePath ((</>))
import System.Process

import Control.Monad
import Control.Exception
import Control.Eternal
import Control.Concurrent

shakeIt :: String → IO ()
shakeIt current = do
  let fullnamelhs = current </> "shake.it.lhs"
      fullnamehs  = current </> "shake.it.hs"
  existslhs ← doesFileExist fullnamelhs
  if existslhs
    then shakeItOff current fullnamelhs
    else do
      existshs ← doesFileExist fullnamehs
      if existshs then shakeItOff current fullnamehs
                  else putStrLn "no shake.it.hs / shake.it.lhs file"

shakeItOff :: String → String → IO ()
shakeItOff dir shakefile = do
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

  getArgs >>= runShake cscr
