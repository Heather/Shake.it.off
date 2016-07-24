{-# LANGUAGE UnicodeSyntax #-}

module Shake.It.C.CMake
  ( cmake
  , build
  ) where

import           Control.Monad
import           Shake.It.Core

cmake ∷ [String] → IO ()
cmake a = rawSystem "cmake" a >>= checkExitCode

build ∷ [String] → IO ()
build α = rawSystem "cmake" ("--build" : α) >>= checkExitCode
