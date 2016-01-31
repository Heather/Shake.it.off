{-# LANGUAGE
    CPP
  , MultiWayIf
  , LambdaCase
  , UnicodeSyntax
  , RankNTypes
  #-}

module Shake.It.Off
  ( shake
  , pony
  , module Shake.It.Core
  , module Shake.It.Haskell
  ) where

import System.Environment

import Control.Monad
import Control.Eternal

import Shake.It.Core
import Shake.It.Haskell

shake :: IO () → IO ()
shake action = action

pony :: String → IO () → IO ()
pony arg action = do
  args ← getArgs
  when (arg ∈ args) action
