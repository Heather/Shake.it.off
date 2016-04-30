{-# LANGUAGE
    UnicodeSyntax
  #-}

module Shake.It.C.CMake
  ( cmake
  ) where

import Control.Monad
import Shake.It.Core

cmake :: [String] → IO ()
cmake a = rawSystem "cmake" a >>= checkExitCode
