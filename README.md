Shake it off
============

Features
--------

 - Simple as pie!
 - You write Haskell, plain haskell, IO (), no Rules, no Actions, no custom stuff to learn, just plain IO ()
 - Compile shake.it.off script only when shake.it really changes!
 - `shake --help` will display `shake.it.hs` options
 - Contains many (and adding) handy functions for creating build scripts everywhere!
 - You can think of pony when you use phony... (it's in early stage)

TODO
----

 - plugin system
 - option for `.atom-build.yml` file generation
 - resolve issues with very complicated dependencies
 - tests
 - document code

Operators
---------

``` haskell
-- operators
infixl 5 ◉
-- even >>= will have 1 priority and $ have priority 0
-- to check priority: type ":i >>=" into ghci
infixl 0 ∰, ∫, #>, ##>, @>, @@>, ♯, ♯♯

-- tuple maker
(◉) ∷ String → [String] → (String, [String])
s ◉ ss = (s, ss)

-- Phony operator
(@>) ∷ String → IO () → IO ()
r @> a = phony r a

-- Phony' operator
(@@>) ∷ (String, [String]) → IO () → IO ()
(r, d) @@> a = phony d r a

-- Unicode variant of phony
(∫) ∷ String → IO () → IO ()
r ∫ a = phony r a

-- Unicode variant of phony'
(∰) ∷ (String, [String]) → IO () → IO ()
(r, d) ∰ a = phony d r a

-- Obj operator
(#>) ∷ String → IO () → IO ()
r #> a = obj r a

-- Obj' operator
(##>) ∷ (String, [String]) → IO () → IO ()
(r, d) ##> a = obj d r a

-- Unicode Obj operator
(♯) ∷ FilePath → IO () → IO ()
r ♯ a = obj r a

-- Unicode Obj' operator
(♯♯) ∷ (FilePath, [String]) → IO () → IO ()
(r, d) ♯♯ a = obj d r a
```

Example
-------

`shake.it.hs` file (or `shake.it.lhs`)

``` haskell
import Shake.It.Off

main :: IO ()
main = shake $ do
  phony "clean" $ cabal ["clean"]

  obj "dist/build/Cr.exe" $ do
    cabal ["install", "--only-dependencies"]
    cabal ["configure"]
    cabal ["build"]
```

every time you run `shake` on this file if `shake.it.off` is outdated or not exists it will be rebuild (otherwise you will just run shake.it.off); when you will run `shake clean` it will process just `cabal clean`, if you will run it with no arguments then it will rebuild project, if you will run it with `shake clean install` it will process `shake clean` first then in case if there will be `shake install` phony it will process it, else way it will process default case (rebuilding) after cleaning. (because it's simple stupid imperative)

more complex example with Unicode operators:

``` haskell
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

```
