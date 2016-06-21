{-# LANGUAGE UnicodeSyntax #-}

module Shake.It.Utils
  ( curl
  ) where

import           Control.Monad
import           Shake.It.Core

curl ∷ [String] → IO ()
curl α = rawSystem "curl" α >>= checkExitCode
