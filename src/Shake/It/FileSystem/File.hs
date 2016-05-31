{-# LANGUAGE Safe          #-}
{-# LANGUAGE UnicodeSyntax #-}

module Shake.It.FileSystem.File
  ( removeIfExists
  ) where

import           Control.Exception
import           Prelude           hiding (catch)
import           System.Directory
import           System.IO.Error   hiding (catch)

removeIfExists ∷ FilePath → IO ()
removeIfExists ζ = removeFile ζ `catch` handleExists
  where handleExists ε
          | isDoesNotExistError ε = return ()
          | otherwise = throwIO ε
