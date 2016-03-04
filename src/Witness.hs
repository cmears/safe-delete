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
findWitness :: FilePath -> FilePath -> IO (Maybe FilePath)
findWitness target baseDir = do
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

  hunt (target, targetStatus) contents

type Target = (FilePath, FileStatus)

hunt :: Target -> [FilePath] -> IO (Maybe FilePath)
hunt _target [] = return Nothing
hunt target  (path:paths) = do
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
  -- If somehow the target and the path are the same file, return
  -- not-a-witness.  This can happen if the potential match is a
  -- hardlink to the target file.
  if fileID targetStatus == fileID pathStatus
    then return False
    else
      -- The file sizes must match.
      case fileSize targetStatus == fileSize pathStatus of
        False -> return False
        True -> do
          -- The file contents must match.
          matchingContents targetPath path

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
  return $! same
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
