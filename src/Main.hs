{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as T
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO.Error
import           System.Posix.Files
import           Turtle.Options

main :: IO ()
main = do
  opts <- options "safe-delete" optionsParser
  print =<< getArgs
  return ()

data Options = Options {
    optionBaseDir :: FilePath
  , optionDryRun :: Bool
}

optString = opt (Just . T.unpack)

optionsParser :: Parser Options
optionsParser =
  Options <$> optString "base" 'b' "Base directory to search for witnesses"
          <*> switch "dry-run" 'n' "Don't delete any files"

-- Finds a witness for the target file in the subtree rooted at the
-- given base directory.  The result is relative to the base
-- directory.
findWitness :: FilePath -> FilePath -> IO (Maybe FilePath)
findWitness target baseDir = do
  baseDirStatus <- modifyIOError
                     (\e -> if ioeGetErrorType e == doesNotExistErrorType
                            then userError "base directory does not exist"
                            else e)
                     (getFileStatus baseDir)
  when (not (isDirectory baseDirStatus)) $ do
    ioError (userError "base directory not a directory")

  targetStatus <- getFileStatus target

  contents <- getDirectoryContentsFiltered baseDir

  hunt (target, targetStatus) contents

type Target = (FilePath, FileStatus)

hunt :: Target -> [FilePath] -> IO (Maybe FilePath)
hunt target [] = return Nothing
hunt target (path:paths) = do
  pathStatus <- getSymbolicLinkStatus path
  -- Completely ignore symbolic links, whether they point to files or
  -- directories.
  case isSymbolicLink pathStatus of
    True -> hunt target paths
    False -> do
      -- Directories get their contents prepended to the search list
      -- (making a depth-first traversal).  Files get checked.
      case isDirectory pathStatus of
        True -> do
          contents <- getDirectoryContentsFiltered path
          hunt target (contents ++ paths)
        False -> do
          result <- check target (path, pathStatus)
          case result of
            True -> return (Just path)
            False -> hunt target paths

-- Does the target match the given path?
check :: Target -> (FilePath, FileStatus) -> IO Bool
check (targetPath, targetStatus) (path, pathStatus) = do
  putStrLn $ "checking " ++ path
  -- If somehow the target and the path are the same file, something
  -- has gone wrong.
  when (fileID targetStatus == fileID pathStatus) $ do
    ioError (userError ("target file found within base directory (" ++ path ++ ")"))

  -- The file sizes must match.
  case fileSize targetStatus == fileSize pathStatus of
    False -> return False
    True -> do
      -- The file contents must match.
      matchingContents targetPath path

-- Are the contents of these two files the same?
matchingContents :: FilePath -> FilePath -> IO Bool
matchingContents path1 path2 = do
  contents1 <- BSL.readFile path1
  contents2 <- BSL.readFile path2
  return (contents1 == contents2)

-- Get the contents of the given directory, removing "." and "..", and
-- prepending the directory path to the results.
--
-- That is, if the path is "abc" and it contains files "def" and
-- "ghi", you'll get back ["abc/def", "abc/ghi"].
getDirectoryContentsFiltered :: FilePath -> IO [FilePath]
getDirectoryContentsFiltered path = do
  map (combine path) . filter f <$> getDirectoryContents path
  where f "." = False
        f ".." = False
        f _ = True
