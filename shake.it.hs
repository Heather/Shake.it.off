{-# LANGUAGE
    UnicodeSyntax
  , OverloadedStrings
  #-}

import Shake.It.Off

main :: IO ()
main = shake $ do
  "clean" ∰ cabal ["clean"]

  "dist/build/shake.exe" ∫ do
    cabal ["install", "--only-dependencies"]
    cabal ["configure"]
    cabal ["build"]

  "install" ∰ -- (["dist/build/shake.exe"])
    cabal ["install"]
