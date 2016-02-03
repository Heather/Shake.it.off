{-# LANGUAGE UnicodeSyntax #-}

import Shake.It.Off

main :: IO ()
main = shake $ do
  -- phony clean @> is non-unicode operator alternative
  "clean" ∫ cabal ["clean"]

  -- building object rule #> is non-unicode operator alternative
  "dist/build/Shake/shake.exe" ♯ do
    cabal ["install", "--only-dependencies"]
    cabal ["configure"]
    cabal ["build"]

  -- install phony depending on obj, @@> is non-unicode operator alternative
  -- ##> or ♯♯ is for dependent object rule, ◉ is just uncarry operator
  "install" ◉ ["dist/build/Shake/shake.exe"] ∰
    cabal ["install"]
