{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_pcc_haskell (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/maxime/source/PolyCubeCounting/haskell/.stack-work/install/x86_64-linux/75fd8e4951838cd19ff034f1d6b677ab2be4fb856638520103bae2e94ff9d516/9.6.4/bin"
libdir     = "/home/maxime/source/PolyCubeCounting/haskell/.stack-work/install/x86_64-linux/75fd8e4951838cd19ff034f1d6b677ab2be4fb856638520103bae2e94ff9d516/9.6.4/lib/x86_64-linux-ghc-9.6.4/pcc-haskell-0.1.0.0-JNyZtksfvoo79sRqDneyQc-pcc-haskell-test"
dynlibdir  = "/home/maxime/source/PolyCubeCounting/haskell/.stack-work/install/x86_64-linux/75fd8e4951838cd19ff034f1d6b677ab2be4fb856638520103bae2e94ff9d516/9.6.4/lib/x86_64-linux-ghc-9.6.4"
datadir    = "/home/maxime/source/PolyCubeCounting/haskell/.stack-work/install/x86_64-linux/75fd8e4951838cd19ff034f1d6b677ab2be4fb856638520103bae2e94ff9d516/9.6.4/share/x86_64-linux-ghc-9.6.4/pcc-haskell-0.1.0.0"
libexecdir = "/home/maxime/source/PolyCubeCounting/haskell/.stack-work/install/x86_64-linux/75fd8e4951838cd19ff034f1d6b677ab2be4fb856638520103bae2e94ff9d516/9.6.4/libexec/x86_64-linux-ghc-9.6.4/pcc-haskell-0.1.0.0"
sysconfdir = "/home/maxime/source/PolyCubeCounting/haskell/.stack-work/install/x86_64-linux/75fd8e4951838cd19ff034f1d6b677ab2be4fb856638520103bae2e94ff9d516/9.6.4/etc"

getBinDir     = catchIO (getEnv "pcc_haskell_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "pcc_haskell_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "pcc_haskell_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "pcc_haskell_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pcc_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pcc_haskell_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
