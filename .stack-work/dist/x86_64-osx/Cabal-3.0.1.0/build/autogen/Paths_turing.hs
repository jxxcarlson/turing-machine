{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_turing (
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

bindir     = "/Users/jxxcarlson/dev/haskell/turing/.stack-work/install/x86_64-osx/e7a7cd0b229058ac2f5f81174943297be34d4b88ea6d178e25b38594190bb795/8.8.4/bin"
libdir     = "/Users/jxxcarlson/dev/haskell/turing/.stack-work/install/x86_64-osx/e7a7cd0b229058ac2f5f81174943297be34d4b88ea6d178e25b38594190bb795/8.8.4/lib/x86_64-osx-ghc-8.8.4/turing-0.1.0.0-EBpKOM7ifd0Ff3hgPs28Ol"
dynlibdir  = "/Users/jxxcarlson/dev/haskell/turing/.stack-work/install/x86_64-osx/e7a7cd0b229058ac2f5f81174943297be34d4b88ea6d178e25b38594190bb795/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/jxxcarlson/dev/haskell/turing/.stack-work/install/x86_64-osx/e7a7cd0b229058ac2f5f81174943297be34d4b88ea6d178e25b38594190bb795/8.8.4/share/x86_64-osx-ghc-8.8.4/turing-0.1.0.0"
libexecdir = "/Users/jxxcarlson/dev/haskell/turing/.stack-work/install/x86_64-osx/e7a7cd0b229058ac2f5f81174943297be34d4b88ea6d178e25b38594190bb795/8.8.4/libexec/x86_64-osx-ghc-8.8.4/turing-0.1.0.0"
sysconfdir = "/Users/jxxcarlson/dev/haskell/turing/.stack-work/install/x86_64-osx/e7a7cd0b229058ac2f5f81174943297be34d4b88ea6d178e25b38594190bb795/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "turing_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "turing_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "turing_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "turing_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "turing_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "turing_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
