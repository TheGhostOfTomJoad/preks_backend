{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_prekParser (
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

bindir     = "/home/max/Dokumente/ws21/preks_backend/.stack-work/install/x86_64-linux-tinfo6/bd5270ea97fc5d96b7204668b8df47381b786902e88fb39697a9c22b86af653e/8.10.7/bin"
libdir     = "/home/max/Dokumente/ws21/preks_backend/.stack-work/install/x86_64-linux-tinfo6/bd5270ea97fc5d96b7204668b8df47381b786902e88fb39697a9c22b86af653e/8.10.7/lib/x86_64-linux-ghc-8.10.7/prekParser-0.1.0.0-HnEp7Tk236SJC2qIViEcTd-prekParser"
dynlibdir  = "/home/max/Dokumente/ws21/preks_backend/.stack-work/install/x86_64-linux-tinfo6/bd5270ea97fc5d96b7204668b8df47381b786902e88fb39697a9c22b86af653e/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/max/Dokumente/ws21/preks_backend/.stack-work/install/x86_64-linux-tinfo6/bd5270ea97fc5d96b7204668b8df47381b786902e88fb39697a9c22b86af653e/8.10.7/share/x86_64-linux-ghc-8.10.7/prekParser-0.1.0.0"
libexecdir = "/home/max/Dokumente/ws21/preks_backend/.stack-work/install/x86_64-linux-tinfo6/bd5270ea97fc5d96b7204668b8df47381b786902e88fb39697a9c22b86af653e/8.10.7/libexec/x86_64-linux-ghc-8.10.7/prekParser-0.1.0.0"
sysconfdir = "/home/max/Dokumente/ws21/preks_backend/.stack-work/install/x86_64-linux-tinfo6/bd5270ea97fc5d96b7204668b8df47381b786902e88fb39697a9c22b86af653e/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "prekParser_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "prekParser_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "prekParser_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "prekParser_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "prekParser_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "prekParser_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
