module Witness where

import           Control.Monad
import qualified Data.ByteString.Lazy as BSL
import           System.Directory
import           System.FilePath
import           System.IO.Error
import           System.Posix.Files

import           System.IO


-- Finds a witness for the target file in the subtree rooted at the
-- given base directory.  The result is relative to the base
-- directory.
findWitness :: Bool -> FilePath -> FilePath -> IO (Maybe FilePath)
findWitness fastMatch target baseDir = do
  baseDirStatus <- modifyIOError
                     (\e -> if ioeGetErrorType e == doesNotExistErrorType
                            then userError "base directory does not exist"
                            else e)
                     (getFileStatus baseDir)
  when (not (isDirectory baseDirStatus)) $ do
    ioError (userError "base directory is not a directory")

  targetStatus <- getSymbolicLinkStatus target
  when (isSymbolicLink targetStatus) $ do
    ioError (userError ("target is symbolic link: " ++ target))

  contents <- getDirectoryContentsFiltered baseDir

  hunt fastMatch (target, targetStatus) contents

type Target = (FilePath, FileStatus)

hunt :: Bool -> Target -> [FilePath] -> IO (Maybe FilePath)
hunt _fastMatch _target [] = return Nothing
hunt fastMatch target  (path:paths) = do
  pathStatus <- getSymbolicLinkStatus path
  -- Completely ignore symbolic links, whether they point to files or
  -- directories.
  case isSymbolicLink pathStatus of
    True -> hunt fastMatch target paths
    False -> do
      -- Directories get their contents prepended to the search list
      -- (making a depth-first traversal).  Files get checked.
      case isDirectory pathStatus of
        True -> do
          contents <- getDirectoryContentsFiltered path
          hunt fastMatch target (contents ++ paths)
        False -> do
          result <- check fastMatch target (path, pathStatus)
          case result of
            True -> return (Just path)
            False -> hunt fastMatch target paths

-- Does the target match the given path?
check :: Bool -> Target -> (FilePath, FileStatus) -> IO Bool
check fastMatch (targetPath, targetStatus) (path, pathStatus) = do
  -- If somehow the target and the path are the same file, return
  -- not-a-witness.  This can happen if the potential match is a
  -- hardlink to the target file.
  if fileID targetStatus == fileID pathStatus
    then return False
    else
      -- The file sizes must match.
      case fileSize targetStatus == fileSize pathStatus of
        False -> return False
        True ->
          if fastMatch
          then return (sameFileName targetPath path)
          else matchingContents targetPath path

sameFileName :: FilePath -> FilePath -> Bool
sameFileName path1 path2 =
  takeFileName path1 == takeFileName path2

-- Are the contents of these two files the same?
matchingContents :: FilePath -> FilePath -> IO Bool
matchingContents path1 path2 = do
  h1 <- openBinaryFile path1 ReadMode
  bs1 <- BSL.hGetContents h1
  h2 <- openBinaryFile path2 ReadMode
  bs2 <- BSL.hGetContents h2
  let same = bs1 == bs2
  -- Forcing the value of "same" will ensure that enough data has been
  -- read, and it's safe to close the file handles.
  void $ return $! same
  hClose h1
  hClose h2
  return same


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

-- Expand the given target into zero or more real targets.  Symbolic
-- links are discarded.  Directories are explored recursively.  Only
-- files are returned.
expandTarget :: FilePath -> IO [FilePath]
expandTarget filepath = do
  pathStatus <- getSymbolicLinkStatus filepath
  case isSymbolicLink pathStatus of
    True -> return []
    False -> do
      case isDirectory pathStatus of
        True -> do
          contents <- getDirectoryContentsFiltered filepath
          concat <$> mapM expandTarget contents
        False -> do
          return [filepath]
