{-# LANGUAGE
    UnicodeSyntax
  #-}

module Shake.It.C.QMake
  ( qmake
  ) where

import Control.Monad
import Shake.It.Core

qmake :: [String] → IO ()
qmake a = rawSystem "qmake" a >>= checkExitCode
