{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Interface.FontLibrary
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for finding and loading fonts on the host system.

module Graphics.Gudni.Interface.FontLibrary
  ( fontLibrary
  , findDefaultFont
  )
where

import System.Info
import Control.Monad
import Data.List

import System.FilePath (addTrailingPathSeparator, normalise)
import System.Directory (getHomeDirectory, getDirectoryContents)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.List (isPrefixOf)


findDefaultFont :: IO String
findDefaultFont = fromMaybe "Times New Roman.ttf" <$> listToMaybe . filter (isInfixOf "Times New Roman.ttf") <$> fontLibrary

-- | Make a relative path absolute on MacOS.
absolutizeMacPath :: String -> IO String
absolutizeMacPath aPath
    | "~" `isPrefixOf` aPath = do
        homePath <- getHomeDirectory
        return $ normalise $ addTrailingPathSeparator homePath
                             ++ tail aPath
    | otherwise = return aPath

-- | Get the default font director based on the host operating system.
fontDirectories =
  case os of
    "darwin" -> mapM absolutizeMacPath ["~/Library/Fonts/", "/Library/Fonts/"]
    _        -> return ["C:\\windows\\fonts\\"]

-- | Get the absolute contents of a director.
absoluteDirectoryContents dir =
  do files <- getDirectoryContents dir
     return $ map (addTrailingPathSeparator dir ++) files

-- | Return a list of loadable font files on the system.
fontLibrary :: IO [String]
fontLibrary =
  do dirs <- fontDirectories
     files <- concat <$> mapM absoluteDirectoryContents dirs
     return $ filter (isSuffixOf "ttf") files
