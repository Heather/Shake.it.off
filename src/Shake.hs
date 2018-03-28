{-# LANGUAGE CPP           #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnicodeSyntax #-}

module Shake
  ( shakeIt
  , shakeItOff
  , module Shake.It.Off
  ) where

import           Script
import           Shake.It.Off

import           System.IO

import           Data.String.Utils
import           Data.Maybe

import           Control.Concurrent
import           Control.Eternal
import           Control.Exception
import           Control.Monad

#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
readCheck    -- return whether command was success or not
  ∷ String   -- command
  → [String] -- arguments
  → IO (Either SomeException String)
readCheck γ args = try $ readProcess γ args []

checkIfSucc           -- check if success
  ∷ String            -- command to check
  → [String]          -- arguments
  → IO (Maybe String) -- Just cmd in case of success
checkIfSucc γ args =
  readCheck γ args
    ≫= \case Left _ → return Nothing
             Right val → do putStr $ γ ⧺ " : " ⧺ val
                            return (Just γ)

versionCheck          -- check for ghc --version
  ∷ String            -- command to check
  → IO (Maybe String) -- Path to GHC in case of success
versionCheck γ = checkIfSucc γ ["--version"]

checkForStackGHC
  ∷ Maybe String
  → IO (Maybe String)
checkForStackGHC γ =
  if isNothing γ
    then do
      localAppData ← getEnv("LOCALAPPDATA")
      let path = localAppData </> "Programs/stack/x86_64-windows/"
      stackPackages ← getDirectoryContents path
      let ghcPackages = filter (startswith "ghc") stackPackages
      stackGHV ←
        case ghcPackages of
          [] → do appData ← getEnv("APPDATA")
                  return $ appData </> "local/bin/ghc.exe"
          xs → let lastGHC = last xs
               in return $ path </> lastGHC </> "bin/ghc.exe"
      versionCheck stackGHV
    else return γ

getGHC ∷ IO String
getGHC = return Nothing ≫= λ "ghc"
                        ≫= checkForStackGHC
                        ≫= \res → return $ fromMaybe "ghc" res
  where λ ∷ String → Maybe String → IO (Maybe String)
        λ χ prev = if isNothing prev
                      then versionCheck χ
                      else return prev
#endif

shakeIt ∷ [String]
        → String  -- current directory
        → Bool    -- force
        → Bool    -- pretend
        → String  -- platform
        → IO ()
shakeIt args current force pretend platform = do
  let fullNamelhs = current </> "shake.it.lhs"
      fullNamehs  = current </> "shake.it.hs"
      shakeShake  = shakeItOff args current force pretend platform
  existslhs ← doesFileExist fullNamelhs
  existshs  ← doesFileExist fullNamehs
  if | existslhs → shakeShake fullNamelhs
     | existshs  → shakeShake fullNamehs
     | otherwise →
        unless ("-h" ∈ args ∨ "--help" ∈ args) $
          putStrLn "no shake.it.hs / shake.it.lhs file"

shakeItOff ∷ [String]
           → String   -- current directory
           → Bool     -- force
           → Bool     -- pretend
           → String   -- platform
           → String   -- shake file
           → IO ()
shakeItOff args dir force pretend platform shakefile = do
#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
  -- on Windows GHC is not possibly in path (specially with stack)
  -- however we can look for it inside stack packages
  ghcCommand ← getGHC
#else
  let ghcCommand = "ghc"
#endif
  let cscr = if | platform ∈ ["Win_x64", "Win"] → "shake.it.off.exe"
                | otherwise → "shake.it.off"

  cscrExists  ← doesFileExist cscr
  doRecompile ←
    if | force → return True
       | cscrExists → do
          scrMTime  ← getMTime shakefile
          cscrMTime ← getMTime cscr
          return $ cscrMTime <= scrMTime
       | otherwise → return True

  when doRecompile $ system (ghcCommand ++ " --make -o " ++ cscr ++ " " ++ shakefile)
                   >>= \case ExitFailure ε → do
                               hPrint stderr ε
                               exitFailure
                             ExitSuccess → return ()

  -- TODO: filter out all the options
  let ifForce =
        if | force → filter (\ο → ο /= "-f"
                               && ο /= "--force") args
           | otherwise → args
      shArgs = filter (not . startswith "--platform") ifForce

  unless pretend $
    runShake cscr shArgs
