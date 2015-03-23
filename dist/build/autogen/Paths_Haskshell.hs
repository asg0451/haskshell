module Paths_Haskshell (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/miles/.cabal/bin"
libdir     = "/home/miles/.cabal/lib/x86_64-linux-ghc-7.8.4/Haskshell-0.1.0.0"
datadir    = "/home/miles/.cabal/share/x86_64-linux-ghc-7.8.4/Haskshell-0.1.0.0"
libexecdir = "/home/miles/.cabal/libexec"
sysconfdir = "/home/miles/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Haskshell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Haskshell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Haskshell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Haskshell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Haskshell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
