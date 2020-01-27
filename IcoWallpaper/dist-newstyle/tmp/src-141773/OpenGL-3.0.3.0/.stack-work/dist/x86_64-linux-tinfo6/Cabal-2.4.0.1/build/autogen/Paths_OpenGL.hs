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

bindir     = "/home/miksu/Haskell/gloss-projects/IcoWallpaper/.stack-work/install/x86_64-linux-tinfo6/e6ad775fd14d6d8eb5cf9452dabd7a808b92676e77b5e6b6aea32f4f21a29050/8.6.5/bin"
libdir     = "/home/miksu/Haskell/gloss-projects/IcoWallpaper/.stack-work/install/x86_64-linux-tinfo6/e6ad775fd14d6d8eb5cf9452dabd7a808b92676e77b5e6b6aea32f4f21a29050/8.6.5/lib/x86_64-linux-ghc-8.6.5/OpenGL-3.0.3.0-6plcthvZfR4zo2NgjeXjY"
dynlibdir  = "/home/miksu/Haskell/gloss-projects/IcoWallpaper/.stack-work/install/x86_64-linux-tinfo6/e6ad775fd14d6d8eb5cf9452dabd7a808b92676e77b5e6b6aea32f4f21a29050/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/miksu/Haskell/gloss-projects/IcoWallpaper/.stack-work/install/x86_64-linux-tinfo6/e6ad775fd14d6d8eb5cf9452dabd7a808b92676e77b5e6b6aea32f4f21a29050/8.6.5/share/x86_64-linux-ghc-8.6.5/OpenGL-3.0.3.0"
libexecdir = "/home/miksu/Haskell/gloss-projects/IcoWallpaper/.stack-work/install/x86_64-linux-tinfo6/e6ad775fd14d6d8eb5cf9452dabd7a808b92676e77b5e6b6aea32f4f21a29050/8.6.5/libexec/x86_64-linux-ghc-8.6.5/OpenGL-3.0.3.0"
sysconfdir = "/home/miksu/Haskell/gloss-projects/IcoWallpaper/.stack-work/install/x86_64-linux-tinfo6/e6ad775fd14d6d8eb5cf9452dabd7a808b92676e77b5e6b6aea32f4f21a29050/8.6.5/etc"

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
