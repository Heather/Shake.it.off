{-# LANGUAGE UnicodeSyntax #-}

  module Shake.It.Haskell.GHC
    ( ghc
    ) where

import           Control.Monad
import           Shake.It.Core

ghc ∷ [String] → IO ()
ghc α = rawSystem "ghc" α >>= checkExitCode
