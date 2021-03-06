{-# LANGUAGE CPP            #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE MultiWayIf     #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE UnicodeSyntax  #-}

import           Shake

import           Foreign.Storable      (sizeOf)

import           System.Console.GetOpt
import           System.IO

import           Control.Applicative
import           Control.Concurrent
import           Control.Eternal
import           Control.Exception
import           Control.Monad

main ∷ IO ()
main = do
  shakeArgs ← getArgs
  current   ← getCurrentDirectory
  let (actions, _, _) = getOpt RequireOrder shakeOptions shakeArgs
  Options { optPlatform = platform
          , optForce = force
          , optPretend = test
          } ← foldl (>>=) (return defaultOptions) actions
  shakeIt shakeArgs current force test platform

data Options = Options
  { optPlatform ∷ String
  , optForce    ∷ Bool
  , optPretend  ∷ Bool
  }

defaultOptions ∷ Options
defaultOptions = Options {
  optPlatform = if | os ∈ ["win32", "mingw32", "cygwin32"] →
                     if sizeOf (undefined :: Int) == 8 then "Win_x64"
                                                       else "Win"
                   | os ∈ ["darwin"] → "Mac"
                   | otherwise → "Linux"
  , optForce    = False
  , optPretend  = False
  }

shakeOptions ∷ [OptDescr (Options → IO Options)]
shakeOptions = [
  Option "v" ["version"]  (NoArg showV) "Display Version",
  Option "h" ["help"]     (NoArg displayHelp) "Display Help",
  Option "p" ["platform"] (ReqArg getp "STRING") "operating system platform",
  Option "f" ["force"]    (NoArg forceRebuild) "force script rebuild",
  Option "P" ["pretend"]  (NoArg pretend) "pretend building (testing shake script)"
  ]

getp ∷ ∀ (m :: * → *). Monad m         ⇒ String → Options → m Options
forceRebuild ∷ ∀ (m ∷ * → *). Monad m ⇒ Options → m Options
pretend      ∷ ∀ (m ∷ * → *). Monad m ⇒ Options → m Options

-- note ο is not o but greek ο instead ^__^
getp arg ο      = return ο { optPlatform = arg }
forceRebuild ο  = return ο { optForce = True }
pretend ο       = return ο { optPretend = True }
displayHelp ο   = do
  prg ← getProgName
  hPutStrLn stderr (usageInfo prg shakeOptions)
  return ο { optForce = True }
