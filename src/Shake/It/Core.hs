{-# LANGUAGE UnicodeSyntax #-}

module Shake.It.Core
  ( checkExitCode
  , exitWithError
  , removePhonyArg
  , compilePhony
  , compileObj
  , module MustHave
  , module Shake.It.Global
  ) where

import           System.Directory   as MustHave
import           System.Environment as MustHave
import           System.Exit        as MustHave
import           System.FilePath    as MustHave
import           System.Info        as MustHave
import           System.Process     as MustHave

import           Data.IORef

import           Control.Eternal
import           Control.Monad

import           Shake.It.Global

exitWithError ∷ String → IO ()
exitWithError μ = do putStrLn $ "Error: " ++ μ
                     exitFailure

checkExitCode ∷ ExitCode → IO ()
checkExitCode ExitSuccess = return ()
checkExitCode (ExitFailure γ) =
    error $ "failed with exit code: " ++ show γ

removePhonyArg ∷ [String] → String → IO [String]
removePhonyArg args arg = do
  let filtered = filter (/= arg) args
  writeIORef phonyArgs filtered
  return filtered

compilePhony ∷ String → IO () → IO ()
compilePhony rule phonyAction = do
  phonyAction
  myPhonyArgs ← readIORef phonyArgs
  when (rule ∈ myPhonyArgs) $ do
    removePhonyArg myPhonyArgs rule
    return ()

compileObj ∷ String → IO () → IO ()
compileObj file buildAction = do
  currentObjectList ← readIORef objectsList
  when (file ∈ currentObjectList) $ do
    currentDir ← getCurrentDirectory
    let fullPath = currentDir </> file
    buildAction -- building this file
    objExists ← doesFileExist fullPath
    unless objExists $ exitWithError (fullPath ++ " doesn't exists")
