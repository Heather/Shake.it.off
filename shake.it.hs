{-# LANGUAGE
    UnicodeSyntax
  #-}

import Shake.It.Off

main :: IO ()
main = shake $ do
  "clean" ∫ cabal ["clean"]

  "dist/build/Shake/shake.exe" ♯ do
    cabal ["install", "--only-dependencies"]
    cabal ["configure"]
    cabal ["build"]

  "install" ◉ ["dist/build/Shake/shake.exe"] ∰
    cabal ["install"]
