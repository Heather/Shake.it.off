{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE UnicodeSyntax #-}

import           Shake.It.Off

main ∷ IO ()
main = shake $ do
  -- phony clean @> is non-unicode operator alternative
  "clean" ∫ cabal ["clean"]

  -- building object rule #> is non-unicode operator alternative
  shakeExecutable ♯ do
    cabal ["install", "--only-dependencies"]
    cabal ["configure"]
    cabal ["build"]

  -- install phony depending on obj, @@> is non-unicode operator alternative
  -- ##> or ♯♯ is for dependent object rule, ◉ is just uncarry operator
  "install" ◉ [shakeExecutable] ∰
    cabal ["install"]

  "test" ◉ [shakeExecutable] ∰
    rawSystem shakeExecutable ["--version"]
      >>= checkExitCode

 where buildPath ∷ String
       buildPath = "dist/build/shake"

       shakeExecutable ∷ String
       shakeExecutable =
         if | os ∈ ["win32", "mingw32", "cygwin32"] → buildPath </> "shake.exe"
            | otherwise → buildPath </> "shake"
