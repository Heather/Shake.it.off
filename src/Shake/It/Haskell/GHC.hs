{-# LANGUAGE
    UnicodeSyntax
  #-}

  module Shake.It.Haskell.GHC
    ( ghc
    ) where

import System.Process

import Control.Monad

import Shake.It.Core

ghc :: [String] → IO ()
ghc a = rawSystem "ghc" a >>= checkExitCode
