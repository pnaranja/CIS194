module Paths_hw3 (
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

bindir     = "C:\\CIS194\\spring13\\hw3\\.stack-work\\install\\8851aa27\\bin"
libdir     = "C:\\CIS194\\spring13\\hw3\\.stack-work\\install\\8851aa27\\lib\\x86_64-windows-ghc-7.10.3\\hw3-0.1.0.0-76Ekkmw4xAX8E0t2cKmmxK"
datadir    = "C:\\CIS194\\spring13\\hw3\\.stack-work\\install\\8851aa27\\share\\x86_64-windows-ghc-7.10.3\\hw3-0.1.0.0"
libexecdir = "C:\\CIS194\\spring13\\hw3\\.stack-work\\install\\8851aa27\\libexec"
sysconfdir = "C:\\CIS194\\spring13\\hw3\\.stack-work\\install\\8851aa27\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw3_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hw3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
