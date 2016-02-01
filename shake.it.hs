{-# LANGUAGE
    UnicodeSyntax
  #-}

import Shake.It.Off

main :: IO ()
main = shake $ do
  phony "clean" $ cabal ["clean"]

  cabal ["install", "--only-dependencies"]
  cabal ["configure"]
  cabal ["build"]
