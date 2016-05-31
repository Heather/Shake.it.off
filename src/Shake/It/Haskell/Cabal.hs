{-# LANGUAGE UnicodeSyntax #-}

module Shake.It.Haskell.Cabal
  ( cabal
  ) where

import           Control.Monad
import           Shake.It.Core

cabal ∷ [String] → IO ()
cabal α = rawSystem "cabal" α >>= checkExitCode
