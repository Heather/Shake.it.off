{-# LANGUAGE
    UnicodeSyntax
  #-}

  module Shake.It.Haskell.Stack
    ( stack
    ) where

import System.Process

import Control.Monad

import Shake.It.Core

stack :: [String] → IO ()
stack a = rawSystem "stack" a >>= checkExitCode
