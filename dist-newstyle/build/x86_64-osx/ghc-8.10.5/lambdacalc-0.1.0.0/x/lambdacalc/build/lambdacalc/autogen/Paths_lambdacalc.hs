{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_lambdacalc (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/saptarishighosh/.cabal/bin"
libdir     = "/Users/saptarishighosh/.cabal/lib/x86_64-osx-ghc-8.10.5/lambdacalc-0.1.0.0-inplace-lambdacalc"
dynlibdir  = "/Users/saptarishighosh/.cabal/lib/x86_64-osx-ghc-8.10.5"
datadir    = "/Users/saptarishighosh/.cabal/share/x86_64-osx-ghc-8.10.5/lambdacalc-0.1.0.0"
libexecdir = "/Users/saptarishighosh/.cabal/libexec/x86_64-osx-ghc-8.10.5/lambdacalc-0.1.0.0"
sysconfdir = "/Users/saptarishighosh/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lambdacalc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lambdacalc_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "lambdacalc_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "lambdacalc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lambdacalc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lambdacalc_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
