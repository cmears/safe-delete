module Files where

import System.Directory
import System.FilePath
import System.Posix.Files

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
