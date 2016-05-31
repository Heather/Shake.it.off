{-# LANGUAGE UnicodeSyntax #-}

  module Shake.It.Haskell.Stack
    ( stack
    ) where

import           Control.Monad
import           Shake.It.Core

stack ∷ [String] → IO ()
stack α = rawSystem "stack" α >>= checkExitCode
