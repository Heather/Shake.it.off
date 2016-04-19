{-# LANGUAGE
    UnicodeSyntax
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
  phonyAction
  myPhonyArgs ← readIORef phonyArgs
  when (rule ∈ myPhonyArgs) $ do
    removePhonyArg myPhonyArgs rule
    return ()

compileObj :: String → IO () → IO ()
compileObj file buildAction = do
  currentObjectList ← readIORef objectsList
  when (file ∈ currentObjectList) $ do
    currentDir ← getCurrentDirectory
    let fullPath = currentDir </> file
    buildAction -- building this file
    objExists ← doesFileExist fullPath
    unless objExists $ exitWithError (fullPath ++ " doesn't exists")
