{-# LANGUAGE
    UnicodeSyntax
  , RankNTypes
  #-}

module Shake.It.Version
  ( showHelp
  , showV
  , printVersion
  ) where

import Text.Printf

import System.Exit
import System.Console.GetOpt

import qualified Paths_ShakeItOff as My
import Data.Version (showVersion)

printVersion :: IO ()
printVersion = putStrLn $ showVersion My.version

showMyV     :: String
showMyV      = showVersion My.version

showV       :: ∀ τ β. τ → IO β
showV _      = printf ("Shake it off v." ++ showMyV) >> exitSuccess

showHelp    :: ∀ τ β α. [OptDescr α] → τ → IO β
showHelp o _ = putStrLn (usageInfo "Usage: shake" o)
                  >> exitSuccess
