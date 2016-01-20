{-# LANGUAGE
    CPP
  , MultiWayIf
  , LambdaCase
  , UnicodeSyntax
  #-}

import Shake

import System.Info (os)
import System.Environment( getArgs )
import System.Exit
import System.Directory
import System.IO
import System.FilePath ((</>))
import System.Process

import Control.Monad
import Control.Exception
import Control.Eternal
import Control.Concurrent

main ∷ IO ()
main = do
  current ← getCurrentDirectory
  let lock = current ++ "shake.it.lock"
  locked ← doesFileExist lock
  let gogo = shakeIt current
      start = myThreadId ≫= \t → withFile lock WriteMode (const gogo)
                                     `finally` removeFile lock
  if locked then do putStrLn "There is already one instance of shake running."
                    putStrLn "Remove lock and start again? (Y/N)"
                    hFlush stdout
                    getLine >>= \case w | w ∈ ["Y", "y"] → start
                                      w | w ∈ ["N", "n"] → return ()
                                      _ → return ()
            else start
