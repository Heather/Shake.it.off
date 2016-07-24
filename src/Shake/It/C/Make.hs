{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Shake.It.C.Make
  ( make
  , configure
  , nmake
  , vcshell
  ) where

import           Control.Monad
import           Shake.It.Core
import           System.Environment

configure ∷ [String] → IO ()
configure α = rawSystem "configure" α >>= checkExitCode

make ∷ [String] → IO ()
make α = rawSystem "make" α >>= checkExitCode

vcshell ∷ [String] → IO String
nmake ∷ [String] → IO ()

#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
vcshell [x] = do
  common ← getEnv $ "VS" ++ x ++ "COMNTOOLS"
  return $ common </> ".."  </> ".."
                  </> "VC"
                  </> "vcvarsall.bat"
vcshell (x:xs) = do
  vcx ← vcshell [x]
  if vcx /= [] then return vcx
               else vcshell xs
vcshell []      = return []

nmake α = rawSystem "nmake" α >>= checkExitCode
#else
vcshell _ = return []
nmake   _ = return ()
#endif
