{-# LANGUAGE
    UnicodeSyntax
  , MultiWayIf
  #-}

module Shake.It.Core
  ( checkExitCode
  , exitWithError
  , removePhonyArg
  , compilePhony
  , compileObj
  , module Shake.It.Global
  ) where

import System.Exit
import System.FilePath
import System.Directory
import System.Info (os)

import Data.IORef

import Control.Monad
import Control.Eternal

import Shake.It.Global

exitWithError :: String → IO ()
exitWithError msg = do putStrLn $ "Error: " ++ msg
                       exitFailure

checkExitCode :: ExitCode → IO ()
checkExitCode ExitSuccess = return ()
checkExitCode (ExitFailure code) =
    error $ "failed with exit code: " ++ show code

removePhonyArg :: [String] → String → IO [String]
removePhonyArg args arg = do
  let filtered = filter (/= arg) args
  writeIORef phonyArgs filtered
  return filtered

compilePhony :: String → IO () → IO ()
compilePhony rule phonyAction = do
  myPhonyArgs ← readIORef phonyArgs
  when (rule ∈ myPhonyArgs) $ do
    phonyAction
    removePhonyArg myPhonyArgs rule
    return ()

compileObj :: String → IO () → IO ()
compileObj file buildAction = do
  currentObjectList ← readIORef objectsList
  when (file ∈ currentObjectList) $ do
    currentDir ← getCurrentDirectory
    let fileOS =
          if | takeExtension file == ".exe" →
                if | os ∈ ["win32", "mingw32", "cygwin32"] → file
                   | otherwise → dropExtension file
             | takeExtension file == ".dll" →
                if | os ∈ ["win32", "mingw32", "cygwin32"] → file
                   | otherwise → addExtension (dropExtension file) ".so"
             | otherwise → file

    let fullPath = currentDir </> fileOS
    buildAction -- building this file
    objExists ← doesFileExist fullPath
    unless objExists $ exitWithError (fullPath ++ " doesn't exists")
