{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_OpenGL (
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
version = Version [3,0,3,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/miksu/.cabal/store/ghc-8.6.5/OpenGL-3.0.3.0-aa01db1f3ff92e42227b115d83c0f98c92549b43fc68acc017177b3b6614dfb6/bin"
libdir     = "/home/miksu/.cabal/store/ghc-8.6.5/OpenGL-3.0.3.0-aa01db1f3ff92e42227b115d83c0f98c92549b43fc68acc017177b3b6614dfb6/lib"
dynlibdir  = "/home/miksu/.cabal/store/ghc-8.6.5/OpenGL-3.0.3.0-aa01db1f3ff92e42227b115d83c0f98c92549b43fc68acc017177b3b6614dfb6/lib"
datadir    = "/home/miksu/.cabal/store/ghc-8.6.5/OpenGL-3.0.3.0-aa01db1f3ff92e42227b115d83c0f98c92549b43fc68acc017177b3b6614dfb6/share"
libexecdir = "/home/miksu/.cabal/store/ghc-8.6.5/OpenGL-3.0.3.0-aa01db1f3ff92e42227b115d83c0f98c92549b43fc68acc017177b3b6614dfb6/libexec"
sysconfdir = "/home/miksu/.cabal/store/ghc-8.6.5/OpenGL-3.0.3.0-aa01db1f3ff92e42227b115d83c0f98c92549b43fc68acc017177b3b6614dfb6/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "OpenGL_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "OpenGL_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "OpenGL_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "OpenGL_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "OpenGL_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "OpenGL_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
