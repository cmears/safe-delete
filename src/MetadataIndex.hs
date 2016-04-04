module MetadataIndex
  ( buildIndex
  , MetadataIndex
  , lookupIndex
  , makeKey
  ) where

import Control.Monad
import qualified Data.Map as M
import System.FilePath
import System.Posix.Files
import System.Posix.Types

import Files

--type Key = (FilePath, FileOffset)
type Key = FileOffset

newtype MetadataIndex = MetadataIndex {
  indexMap :: M.Map Key [FilePath]
} deriving (Show)

emptyIndex :: MetadataIndex
emptyIndex = MetadataIndex M.empty

addToIndex :: Key -> FilePath -> MetadataIndex -> MetadataIndex
addToIndex key val index =
  MetadataIndex (M.insertWith (++) key [val] (indexMap index))

lookupIndex :: Key -> MetadataIndex -> [FilePath]
lookupIndex key index =
  M.findWithDefault [] key (indexMap index)

makeKey :: FilePath -> FileStatus -> Key
makeKey path pathStatus =
--  (takeFileName path, fileSize pathStatus)
  fileSize pathStatus

buildIndex :: FilePath -> IO MetadataIndex
buildIndex baseDirPath = do
  buildIndex' baseDirPath emptyIndex

-- Add a path and its descendents to the index.  Symbolic links are
-- ignored.
buildIndex' :: FilePath -> MetadataIndex -> IO MetadataIndex
buildIndex' path index = do
  pathStatus <- getSymbolicLinkStatus path
  -- Completely ignore symbolic links, whether they point to files or
  -- directories.
  case isSymbolicLink pathStatus of
    True -> pure index
    False -> do
      case isDirectory pathStatus of
        True -> do
          contents <- getDirectoryContentsFiltered path
          foldM (\ix p -> buildIndex' p ix) index contents
        False -> do
          return (addToIndex (makeKey path pathStatus) path index)
