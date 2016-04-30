{-# LANGUAGE
    UnicodeSyntax
  #-}

module Shake.It.C.Make
  ( make
  , configure
  ) where

import Control.Monad
import Shake.It.Core

configure :: [String] → IO ()
configure a = rawSystem "configure" a >>= checkExitCode

make :: [String] → IO ()
make a = rawSystem "make" a >>= checkExitCode
