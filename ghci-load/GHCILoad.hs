{-# LANGUAGE UnicodeSyntax #-}

module GHCILoad where

import Control.Monad
import Data.List
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Compiler
import Distribution.Verbosity
import Language.Haskell.Extension
import System.Directory
import System.Environment
import System.Process
import System.Exit
import System.IO

main ∷ IO ExitCode
main = do
  args ← getArgs
  case args of
    ["help"] → do
      putStrLn $ intercalate "\n"
        [ "usage:"
        , "<cmd> [<path>] -- execute ghci with sandbox package-db"
        , "               -- cabal file extensions and options, and load <path>"
        , "<cmd> help     -- show this message"
        ]
      exitWith ExitSuccess
    _ → return ()
  bi ← getBuildInfo
  ppM ← getPackagePath
  let script = case args of
        [] → intercalate "\n" $
          [ ghciExtensionCommands bi
          , ghciOptionCommands bi
          ]
        [lfn] → intercalate "\n" $
          [ ghciExtensionCommands bi
          , ghciOptionCommands bi
          , ghciLoadCommand lfn
          ]
      cmd fn = "ghci -ghci-script=" ++ fn ++ case ppM of
        Nothing → ""
        Just pp → " -package-db=" ++ pp
  (fn,h) ← openTempFile "." "ghci-exts.tmp"
  hPutStrLn h script
  hClose h
  ec ← system $ cmd fn
  removeFile fn
  return ec

getLocalCabalFile ∷ IO String
getLocalCabalFile = do
  fs ← liftM (filter (".cabal" `isSuffixOf`)) $ getDirectoryContents "."
  case fs of
    [] → fail "no local cabal file"
    [fn] → return fn
    fs → fail $ "multiple cabal files: " ++ show fs

getBuildInfo ∷ IO BuildInfo
getBuildInfo = do
  fn ← getLocalCabalFile
  gp ← readPackageDescription silent fn
  case (libBuildInfo . condTreeData) `fmap` condLibrary gp of
    Nothing → fail "no library in cabal file"
    Just bi → return bi

getPackagePath ∷ IO (Maybe String)
getPackagePath = do
  (code,out,err) ← readCreateProcessWithExitCode (shell "cat cabal.sandbox.config | grep package-db") ""
  case code of
    ExitSuccess → case words out of
      [_,path] → return $ Just path
    ExitFailure _ → return Nothing

extensionCommand ∷ Extension → String
extensionCommand (EnableExtension ex) = show ex
extensionCommand (DisableExtension ex) = "No" ++ show ex

ghcExtensionFlags ∷ BuildInfo → String
ghcExtensionFlags = intercalate " " . map (\ s → "-X" ++ extensionCommand s) . defaultExtensions

ghciExtensionCommands ∷ BuildInfo → String
ghciExtensionCommands = intercalate "\n" . map (\ s → ":set -X" ++ extensionCommand s) . defaultExtensions

ghciOptionCommands ∷ BuildInfo → String
ghciOptionCommands bi = 
  let ops = case lookup GHC $ options bi of
        Nothing → []
        Just os → os
  in intercalate "\n" $ map (\ s → ":set " ++ show s) ops

ghciPrettyPrintCommand ∷ String → String
ghciPrettyPrintCommand pp = ":set -interactive-print=" ++ pp

ghciLoadCommand ∷ String → String
ghciLoadCommand m = ":load " ++ m
