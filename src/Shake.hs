{-# LANGUAGE
    CPP
  , MultiWayIf
  , LambdaCase
  , UnicodeSyntax
  , RankNTypes
  #-}

module Shake
  ( shakeIt
  , shake
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
  let fullname = current </> "shake.it.hs"
  exists ← doesFileExist fullname
  if exists then shake current fullname
            else putStrLn "no shake.it.hs file"

shake :: String → String → IO ()
shake dir shakefile = do
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

  shakeItOff cscr
