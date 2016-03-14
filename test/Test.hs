import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Data.List
import System.FilePath
import System.IO
import System.IO.Error
import System.IO.Temp hiding (openNewBinaryFile)
import System.Posix
import System.Random

import Witness

main :: IO ()
main = do
  test1
  test2
  test3
  test4
  test5
  test6
  test7
  test8

-- Wrapper around inTempDirectory' which uses a default path and
-- pattern, and handles exceptions such that the temporary directory
-- is always removed.
inTempDirectory :: IO () -> IO ()
inTempDirectory action = do
  inTempDirectory' "." "tempXXX"
    (catchIOError action (\e -> do
      hPutStrLn stderr (show e)
      return ()))

-- Wrapper around withTempDirectory which changes into the directory
-- to run the action, then restores the previous working directory.
inTempDirectory' :: FilePath -> String -> IO () -> IO ()
inTempDirectory' path pattern action = do
  olddir <- getWorkingDirectory
  withTempDirectory path pattern $ \tempdir -> do
    changeWorkingDirectory tempdir
    action
    changeWorkingDirectory olddir

-- Simple test: make sure we can find a witness.
test1 :: IO ()
test1 = do
  inTempDirectory $ do
    createDirectory "dir" ownerModes
    createRandomFile ("dir" </> "abc")
    copyFile ("dir" </> "abc") "def"
    shouldBeJust =<< findWitness False "def" "dir"
    putStrLn "test1 OK"

-- Make sure that file contents are checked.  (Makes a proper witness,
-- but then alters one byte so they don't match.)
test2 :: IO ()
test2 = do
  inTempDirectory $ do
    createDirectory "dir" ownerModes
    createRandomFile ("dir" </> "abc")
    createLink ("dir" </> "abc") ("dir" </> "link")
    copyFile ("dir" </> "abc") "def"
    alterOneByte "def"
    shouldBeNothing =<< findWitness False "def" "dir"
    putStrLn "test2 OK"

-- Make sure we don't use a symbolic link as a witness.
test3 :: IO ()
test3 = do
  inTempDirectory $ do
    createDirectory "dir" ownerModes
    createRandomFile "abc"
    createSymbolicLink (".." </> "abc") ("dir" </> "link")
    copyFile "abc" "def"
    shouldBeNothing =<< findWitness False "def" "dir"
    putStrLn "test3 OK"

-- Make sure we don't use a hard link as a witness.
test4 :: IO ()
test4 = do
  inTempDirectory $ do
    createDirectory "dir" ownerModes
    createRandomFile "abc"
    createLink "abc" ("dir" </> "link")
    shouldBeNothing =<< findWitness False "abc" "dir"
    putStrLn "test4 OK"

-- Make sure we don't follow symlinks to directories during witness
-- search.
test5 :: IO ()
test5 = do
  inTempDirectory $ do
    createDirectory "dir" ownerModes
    createRandomFile ("dir" </> "abc")
    createDirectory "base" ownerModes
    createSymbolicLink (".." </> "dir" </> "abc") ("base" </> "link")
    copyFile ("dir" </> "abc") "target"
    shouldBeNothing =<< findWitness False "target" "base"
    putStrLn "test5 OK"

-- Make sure we don't use a file as its own witness.
test6 :: IO ()
test6 = do
  inTempDirectory $ do
    createDirectory "dir" ownerModes
    createRandomFile ("dir" </> "abc")
    shouldBeNothing =<< findWitness False ("dir" </> "abc") "dir"
    putStrLn "test6 OK"

-- Make sure fast checking doesn't look at file contents.
test7 :: IO ()
test7 = do
  inTempDirectory $ do
    createDirectory "dir" ownerModes
    createRandomFile ("dir" </> "abc")
    createRandomFile "abc"
    shouldBeJust =<< findWitness True "abc" "dir"
    putStrLn "test7 OK"

-- Make sure expandTarget gives the right result.
test8 :: IO ()
test8 = do
  inTempDirectory $ do
    createDirectory "dir" ownerModes
    createRandomFile ("dir" </> "abc")
    createSymbolicLink "abc" ("dir" </> "link")
    createDirectory ("dir" </> "dir2") ownerModes
    createRandomFile ("dir" </> "dir2" </> "def")
    paths <- expandTarget "dir"
    shouldBeEqual (sort paths) (sort ["dir/abc","dir/dir2/def"])
    putStrLn "test8 OK"

-- Creates a file of 1024 random bytes at the given path.
createRandomFile :: FilePath -> IO ()
createRandomFile path = do
  handle <- openNewBinaryFile path
  contents <- BSL.pack <$> replicateM 1024 randomIO
  BSL.hPut handle contents
  hClose handle

-- Copies a file from src to dst, possibly overwriting dst.
copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst = do
  srcHandle <- openBinaryFile src ReadMode
  dstHandle <- openNewBinaryFile dst
  contents <- BSL.hGetContents srcHandle
  BSL.hPut dstHandle contents
  hClose dstHandle
  hClose srcHandle

-- Open path for writing in binary mode, but throws error if a file
-- already exists at that path.  Has potential race condition between
-- existence-check and opening file.
openNewBinaryFile :: FilePath -> IO Handle
openNewBinaryFile path = do
  exists <- fileExist path
  if exists
    then throwIO (userError ("openNewBinaryFile: already exists: " ++ path))
    else openBinaryFile path WriteMode

shouldBeNothing :: Show a => Maybe a -> IO ()
shouldBeNothing Nothing = return ()
shouldBeNothing (Just x) = do
  throwIO (userError ("error: shouldBeNothing: " ++ show (Just x)))
  
shouldBeJust :: Maybe a -> IO ()
shouldBeJust (Just _x) = return ()
shouldBeJust Nothing = do
  throwIO (userError ("error: shouldBeJust: Nothing"))

shouldBeEqual :: (Eq a, Show a) => a -> a -> IO ()
shouldBeEqual x y =
  if x == y
  then return ()
  else throwIO (userError ("error: shouldBeEqual: " ++ show x ++ " and " ++ show y))

-- Change one byte in a file so that the result is guaranteed to be
-- different from before.  File must have at least 101 bytes for this
-- to work.  Reads the entire file into memory.
alterOneByte :: FilePath -> IO ()
alterOneByte path = do
  contents <- BSL.readFile path
  let contents' = BSL.concat [ BSL.take 100 contents
                             , BSL.singleton ((contents `BSL.index` 100) + 1)
                             , BSL.drop 101 contents
                             ]
  -- This forces the contents of the file to be read completely, which
  -- causes the file to be closed, which allows us to open it again
  -- for writing.
  void $ return $! BSL.length contents
  BSL.writeFile path contents'

