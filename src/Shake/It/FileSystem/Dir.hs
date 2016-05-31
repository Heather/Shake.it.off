{-# LANGUAGE Safe          #-}
{-# LANGUAGE UnicodeSyntax #-}

module Shake.It.FileSystem.Dir
  ( removeDirIfExists
  , copyDir
  ) where

import           Control.Exception
import           Control.Monad          (forM_)
import           Prelude                hiding (catch)
import           System.Directory
import           System.IO.Error        hiding (catch)

import           System.FilePath        ((</>))

import           Control.Eternal.Syntax

removeDirIfExists ∷ FilePath → IO ()
removeDirIfExists δ = removeDirectoryRecursive δ `catch` handleExists
  where handleExists ε
          | isDoesNotExistError ε = return ()
          | otherwise = throwIO ε

copyDir ∷ FilePath -- source
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
