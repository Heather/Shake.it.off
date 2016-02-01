{-# LANGUAGE
    UnicodeSyntax
  #-}

  module Shake.It.Haskell.Cabal
    ( cabal
    ) where

import System.Process

import Control.Monad

import Shake.It.Core

cabal :: [String] → IO ()
cabal a = rawSystem "cabal" a >>= checkExitCode
