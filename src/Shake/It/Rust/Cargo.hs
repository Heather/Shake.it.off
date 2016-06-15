{-# LANGUAGE UnicodeSyntax #-}

module Shake.It.Rust.Cargo
  ( cargo
  ) where

import           Control.Monad
import           Shake.It.Core

cargo ∷ [String] → IO ()
cargo α = rawSystem "cargo" α >>= checkExitCode
