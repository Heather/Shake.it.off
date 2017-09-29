{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE UnicodeSyntax #-}

import           Shake.It.Off

main ∷ IO ()
main = shake $ do
  -- phony clean @> is non-unicode operator alternative
  "clean | clean the project" ∫ cabal ["clean"]

  -- building object rule #> is non-unicode operator alternative
  shakeExecutable ♯ do
    cabal ["install", "--only-dependencies"]
    cabal ["configure"]
    cabal ["build"]

  -- install phony depending on obj, @@> is non-unicode operator alternative
  -- ##> or ♯♯ is for dependent object rule, ◉ is just uncarry operator
  "install | install to system" ◉ [shakeExecutable] ∰
    cabal ["install"]

  "test | build and test" ◉ [shakeExecutable] ∰
    rawSystem shakeExecutable ["--version"]
      >>= checkExitCode

  "rebuild | clean and rebuild" ◉ ["clean"] ∰ do
    cabal ["configure"]
    cabal ["build"]

 where buildPath ∷ String
       buildPath = "dist/build/shake"

       shakeExecutable ∷ String
       shakeExecutable =
         if | os ∈ ["win32", "mingw32", "cygwin32"] → buildPath </> "shake.exe"
            | otherwise → buildPath </> "shake"
