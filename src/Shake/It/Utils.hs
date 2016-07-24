{-# LANGUAGE UnicodeSyntax #-}

module Shake.It.Utils
  ( curl
  , git
  ) where

import           Control.Monad
import           Shake.It.Core

curl ∷ [String] → IO ()
curl α = rawSystem "curl" α >>= checkExitCode

git ∷ [String] → IO ()
git α = rawSystem "git" α >>= checkExitCode
