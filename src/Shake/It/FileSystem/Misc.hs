{-# LANGUAGE
    UnicodeSyntax
  , CPP
  #-}

module Shake.It.FileSystem.Misc
  ( fixUnicodeOnWindows
  ) where

import System.Process
import Control.Monad
import Shake.It.Core

fixUnicodeOnWindows :: IO ()
fixUnicodeOnWindows =
#if ! ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
  rawSystem "chcp" ["65001"] >>= checkExitCode
#else
  return ()
#endif
