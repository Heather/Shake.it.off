{-# LANGUAGE UnicodeSyntax #-}

module Shake.It.C.QMake
  ( qmake
  ) where

import           Control.Monad
import           Shake.It.Core

qmake ∷ [String] → IO ()
qmake α = rawSystem "qmake" α >>= checkExitCode
