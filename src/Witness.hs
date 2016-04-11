module Witness where

import           Control.Monad
import qualified Data.ByteString.Lazy as BSL
import           System.FilePath
import           System.IO.Error
import           System.Posix.Files
import           System.IO

import           Files
import           MetadataIndex


-- Finds a witness for the target file in the subtree rooted at the
-- given base directory.  The result is relative to the base
-- directory.
findWitness :: Bool -> Maybe MetadataIndex -> FilePath -> FilePath -> IO (Maybe FilePath)
findWitness fastMatch maybeIndex target baseDir = do
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

  case maybeIndex of
    -- No index: ordinary filesystem traversal
    Nothing -> hunt fastMatch (target, targetStatus) contents
    -- Index: use it to find potential matches
    Just idx ->
      let potentialMatches = lookupIndex (makeKey target targetStatus) idx
      in findM (\pm -> do
                  pmStatus <- getSymbolicLinkStatus pm
                  check fastMatch (target, targetStatus) (pm, pmStatus))
           potentialMatches

-- Find the first element for which the monadic predicate returns
-- True.  Stops executing after finding the first such element.
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _f [] = return Nothing
findM  f (a:as) = f a >>= \b -> if b then return (Just a) else findM f as

type Target = (FilePath, FileStatus)

hunt :: Bool -> Target -> [FilePath] -> IO (Maybe FilePath)
hunt _fastMatch _target [] = return Nothing
hunt  fastMatch  target (path:paths) = do
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
  if      fileID targetStatus ==   fileID pathStatus
     && deviceID targetStatus == deviceID pathStatus
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

