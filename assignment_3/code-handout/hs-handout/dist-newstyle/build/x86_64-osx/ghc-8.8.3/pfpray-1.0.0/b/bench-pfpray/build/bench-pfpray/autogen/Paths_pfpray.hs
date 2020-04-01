{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_pfpray (
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
version = Version [1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/philiplassen/.cabal/bin"
libdir     = "/Users/philiplassen/.cabal/lib/x86_64-osx-ghc-8.8.3/pfpray-1.0.0-inplace-bench-pfpray"
dynlibdir  = "/Users/philiplassen/.cabal/lib/x86_64-osx-ghc-8.8.3"
datadir    = "/Users/philiplassen/.cabal/share/x86_64-osx-ghc-8.8.3/pfpray-1.0.0"
libexecdir = "/Users/philiplassen/.cabal/libexec/x86_64-osx-ghc-8.8.3/pfpray-1.0.0"
sysconfdir = "/Users/philiplassen/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "pfpray_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "pfpray_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "pfpray_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "pfpray_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pfpray_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pfpray_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
