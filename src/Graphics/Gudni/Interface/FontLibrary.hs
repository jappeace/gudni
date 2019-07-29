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

import Control.Exception
import System.Info
import Control.Monad
import Data.List

import System.FilePath (addTrailingPathSeparator, normalise)
import System.Directory (getHomeDirectory, getDirectoryContents)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.List (isPrefixOf)


findDefaultFont :: IO String
findDefaultFont = do
  fonts <- fontLibrary
  case (listToMaybe $ filter (isInfixOf "Times New Roman.ttf") fonts) of
    Just x -> pure x
    Nothing -> 
      maybe (do
                putStrLn "can't find Times New Roman, choosing first available"
                print fonts
                pure $ head $ fonts
            )  pure $ listToMaybe $ filter (isInfixOf "Times_New_Roman.ttf") fonts

-- | Make a relative path absolute on MacOS.
absolutizeMacPath :: String -> IO String
absolutizeMacPath aPath
    | "~" `isPrefixOf` aPath = do
        homePath <- getHomeDirectory
        return $ normalise $ addTrailingPathSeparator homePath
                             ++ tail aPath
    | otherwise = return aPath

-- | Get the default font director based on the host operating system.
fontDirectories = -- maybe use http://hackage.haskell.org/package/load-font ?
  case os of
    "darwin" -> mapM absolutizeMacPath ["~/Library/Fonts/", "/Library/Fonts/"]
    "linux" -> mapM absolutizeMacPath ["/usr/share/fonts/truetype/liberation/", "/usr/share/fonts/truetype/msttcorefonts/"]
    _        -> return ["C:\\windows\\fonts\\"]

-- | Get the absolute contents of a director.
absoluteDirectoryContents :: FilePath -> IO [String]
absoluteDirectoryContents dir =
  do files <- getDirectoryContents dir
     return $ map (addTrailingPathSeparator dir ++) files

printException :: FilePath -> IOException -> IO [String]
printException dir x = do
    putStrLn $ " ignoring directory" <> dir <> "because " <> show x
    pure mempty

-- | Return a list of loadable font files on the system.
fontLibrary :: IO [String]
fontLibrary =
  do dirs <- fontDirectories
     files <- fmap concat $ traverse (\dir ->
                catch (absoluteDirectoryContents dir)
                      (printException dir)) dirs

     return $ filter (isSuffixOf "ttf") files
