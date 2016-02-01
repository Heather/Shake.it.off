{-# LANGUAGE
    UnicodeSyntax
  #-}

import System.Exit
import System.Process

import Data.List

import Control.Monad

import Shake.It.Off

main :: IO ()
main = shake $ do
  pony "clean" $ cabal ["clean"]

  cabal ["install", "--only-dependencies"]
  cabal ["configure"]
  cabal ["build"]
