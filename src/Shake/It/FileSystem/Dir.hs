{-# LANGUAGE
    UnicodeSyntax
  , Safe
  #-}

module Shake.It.FileSystem.Dir
  ( removeDirIfExists
  , copyDir
  ) where

import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
import Control.Monad(forM_)

import System.FilePath((</>))

import Control.Eternal.Syntax

removeDirIfExists :: FilePath → IO ()
removeDirIfExists dirName = removeDirectoryRecursive dirName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

copyDir :: FilePath -- source
         → FilePath -- destination
         → IO ()
copyDir src dst = do
  createDirectory dst
  content ← getDirectoryContents src
  let xs = filter (∉ [".", ".."]) content
  forM_ xs $ \name → let srcPath = src </> name
                         dstPath = dst </> name
      in doesDirectoryExist srcPath >>= \dirExist →
          if dirExist then copyDir srcPath dstPath
                      else copyFile srcPath dstPath
