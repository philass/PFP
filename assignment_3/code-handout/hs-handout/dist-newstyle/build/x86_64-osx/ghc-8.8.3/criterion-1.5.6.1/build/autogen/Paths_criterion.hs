{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_criterion (
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
version = Version [1,5,6,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/philiplassen/.cabal/bin"
libdir     = "/Users/philiplassen/.cabal/lib/x86_64-osx-ghc-8.8.3/criterion-1.5.6.1-inplace"
dynlibdir  = "/Users/philiplassen/.cabal/lib/x86_64-osx-ghc-8.8.3"
datadir    = "/Users/philiplassen/.cabal/share/x86_64-osx-ghc-8.8.3/criterion-1.5.6.1"
libexecdir = "/Users/philiplassen/.cabal/libexec/x86_64-osx-ghc-8.8.3/criterion-1.5.6.1"
sysconfdir = "/Users/philiplassen/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "criterion_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "criterion_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "criterion_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "criterion_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "criterion_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "criterion_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
