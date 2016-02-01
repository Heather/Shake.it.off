{-# LANGUAGE
    UnicodeSyntax
  #-}

  module Shake.It.Core
    ( checkExitCode
    ) where

import System.Exit

checkExitCode :: ExitCode → IO ()
checkExitCode ExitSuccess = return ()
checkExitCode (ExitFailure code) =
    error $ "failed with exit code: " ++ show code
