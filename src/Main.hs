{-# LANGUAGE
    CPP
  , MultiWayIf
  , LambdaCase
  , UnicodeSyntax
  , RankNTypes
  , KindSignatures
  #-}

import Shake

import Foreign.Storable (sizeOf)

import System.Console.GetOpt
import System.IO

import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Eternal
import Control.Concurrent

main ∷ IO ()
main = do
  shakeArgs ← getArgs
  current   ← getCurrentDirectory
  let (actions, _, _) = getOpt RequireOrder shakeOptions shakeArgs
  Options { optPlatform = platform, optForce = force
          } ← foldl (>>=) (return defaultOptions) actions
  shakeIt shakeArgs current force platform

data Options = Options
  { optPlatform ∷ String
  , optForce    ∷ Bool
  }

defaultOptions ∷ Options
defaultOptions = Options {
  optPlatform = if | os ∈ ["win32", "mingw32", "cygwin32"] →
                     if sizeOf (undefined :: Int) == 8 then "Win_x64"
                                                       else "Win"
                   | os ∈ ["darwin"] → "Mac"
                   | otherwise → "Linux"
  , optForce = False
  }

shakeOptions ∷ [OptDescr (Options → IO Options)]
shakeOptions = [
  Option "v" ["version"]  (NoArg showV) "Display Version",
  Option "h" ["help"]     (NoArg displayHelp) "Display Help",
  Option "p" ["platform"] (ReqArg getp "STRING") "operating system platform",
  Option "f" ["force"]    (NoArg forceRebuild) "force script rebuild"
  ]

getp :: ∀ (m :: * → *). Monad m         ⇒ String → Options → m Options
forceRebuild :: ∀ (m :: * → *). Monad m ⇒ Options → m Options

getp arg opt      = return opt { optPlatform = arg }
forceRebuild opt  = return opt { optForce = True }
displayHelp opt   = do
  prg ← getProgName
  hPutStrLn stderr (usageInfo prg shakeOptions)
  return opt { optForce = True }
