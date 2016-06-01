{-# LANGUAGE UnicodeSyntax #-}

module Shake.It.C.Make
  ( make
  , nmake
  , configure
  ) where

import           Control.Monad
import           Shake.It.Core

configure ∷ [String] → IO ()
configure α = rawSystem "configure" α >>= checkExitCode

make ∷ [String] → IO ()
make α = rawSystem "make" α >>= checkExitCode

nmake ∷ [String] → IO ()
nmake α = rawSystem "nmake" α >>= checkExitCode
