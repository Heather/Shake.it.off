{-# LANGUAGE UnicodeSyntax #-}

module Shake.It.Core
  ( checkExitCode
  , exitWithError
  , nameAndDesc
  , descByname
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

import           Data.Char (isSpace)
import           Data.List.Split
import           Data.IORef

import           Control.Eternal
import           Control.Monad

import           Shake.It.Global

exitWithError ∷ String → IO ()
exitWithError μ = do putStrLn $ "Error: " ++ μ
                     exitFailure

trim ∷ String → String
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail ∷ String → String → String
dropSpaceTail _ "" = ""
dropSpaceTail maybeStuff (χ:xs)
  | isSpace χ       = dropSpaceTail (χ:maybeStuff) xs
  | null maybeStuff = χ : dropSpaceTail "" xs
  | otherwise       = reverse maybeStuff ++ χ : dropSpaceTail "" xs

nameAndDesc ∷ [String] → [(String, String)]
nameAndDesc [] = []
nameAndDesc [χ] =
  let splt = splitOn "|" χ
  in if length splt > 2
      then [(trim (head splt), last splt)]
      else [(trim χ, "No description")]
nameAndDesc (χ:xs) = nameAndDesc [χ] ++ nameAndDesc xs

descByname ∷ [(String, String)] → String → String
descByname [] _ = []
descByname xs x =
  let filtered = filter (\(a,d) → a /= x) xs
  in snd $ head filtered

checkExitCode ∷ ExitCode → IO ()
checkExitCode ExitSuccess = return ()
checkExitCode (ExitFailure γ) =
    error $ "failed with exit code: " ++ show γ

removePhonyArg ∷ [(String, String)] → String → IO [(String, String)]
removePhonyArg args arg = do
  let filtered = filter (\(a,d) → a /= arg) args
  writeIORef phonyArgs filtered
  return filtered

compilePhony ∷ String → IO () → IO ()
compilePhony rule phonyAction = do
  phonyAction
  myPhonyArgs ← readIORef phonyArgs
  let args = map fst myPhonyArgs
  when (rule ∈ args) $ do
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
