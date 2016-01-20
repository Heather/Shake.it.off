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
  ) where

import System.Environment

import Control.Monad
import Control.Eternal

shake :: IO () → IO ()
shake action = action

pony :: String → IO () → IO ()
pony arg action = do
  args <- getArgs
  when (arg ∈ args) action
