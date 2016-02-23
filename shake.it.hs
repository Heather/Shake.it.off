{-# LANGUAGE UnicodeSyntax #-}

import Shake.It.Off

import System.Process

main :: IO ()
main = shake $ do
  -- phony clean @> is non-unicode operator alternative
  "clean" ∫ cabal ["clean"]

  -- building object rule #> is non-unicode operator alternative
  buildPath </> "shake.exe" ♯ do
    cabal ["install", "--only-dependencies"]
    cabal ["configure"]
    cabal ["build"]

  -- install phony depending on obj, @@> is non-unicode operator alternative
  -- ##> or ♯♯ is for dependent object rule, ◉ is just uncarry operator
  "install" ◉ [buildPath </> "shake.exe"] ∰
    cabal ["install"]

  "test" ◉ [buildPath </> "shake.exe"] ∰ do
    rawSystem (buildPath </> "shake.exe") ["--version"]
      >>= checkExitCode

 where buildPath :: String
       buildPath = "dist/build/Shake"
