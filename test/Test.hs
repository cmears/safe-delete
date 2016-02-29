import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import System.FilePath
import System.IO
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

-- Simple test: make sure we can find a witness.
test1 :: IO ()
test1 = do
  withTempDirectory "." "tempXXX" $ \tempdir -> do
    changeWorkingDirectory tempdir
    createDirectory "dir" ownerModes
    createRandomFile ("dir" </> "abc")
    copyFile ("dir" </> "abc") "def"
    shouldBeJust =<< findWitness "def" "dir"
  putStrLn "test1 OK"

-- Make sure that file contents are checked.  (Makes a proper witness,
-- but then alters one byte so they don't match.)
test2 :: IO ()
test2 = do
  withTempDirectory "." "tempXXX" $ \tempdir -> do
    changeWorkingDirectory tempdir
    createDirectory "dir" ownerModes
    createRandomFile ("dir" </> "abc")
    createLink ("dir" </> "abc") ("dir" </> "link")
    copyFile ("dir" </> "abc") "def"
    alterOneByte "def"
    shouldBeNothing =<< findWitness "def" "dir"
  putStrLn "test2 OK"

-- Make sure we don't use a symbolic link as a witness.
test3 :: IO ()
test3 = do
  withTempDirectory "." "tempXXX" $ \tempdir -> do
    changeWorkingDirectory tempdir
    createDirectory "dir" ownerModes
    createRandomFile "abc"
    createSymbolicLink ("abc") ("dir" </> "link")
    copyFile "abc" "def"
    shouldBeNothing =<< findWitness "def" "dir"
  putStrLn "test3 OK"

-- Make sure we don't use a hard link as a witness.
test4 :: IO ()
test4 = do
  withTempDirectory "." "tempXXX" $ \tempdir -> do
    changeWorkingDirectory tempdir
    createDirectory "dir" ownerModes
    createRandomFile "abc"
    createLink "abc" ("dir" </> "link")
    shouldBeNothing =<< findWitness "abc" "dir"
  putStrLn "test4 OK"

createRandomFile :: FilePath -> IO ()
createRandomFile path = do
  handle <- openNewBinaryFile path
  contents <- BSL.pack <$> replicateM 1024 randomIO
  BSL.hPut handle contents
  hClose handle

copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst = do
  srcHandle <- openBinaryFile src ReadMode
  dstHandle <- openNewBinaryFile dst
  contents <- BSL.hGetContents srcHandle
  BSL.hPut dstHandle contents
  hClose dstHandle
  hClose srcHandle

openNewBinaryFile :: FilePath -> IO Handle
openNewBinaryFile path = do
  exists <- fileExist path
  if exists
    then error ("openNewBinaryFile: already exists: " ++ path)
    else openBinaryFile path WriteMode

shouldBeNothing :: Show a => Maybe a -> IO ()
shouldBeNothing Nothing = return ()
shouldBeNothing (Just x) = do
  error ("error: shouldBeNothing: " ++ show (Just x))
  
shouldBeJust :: Maybe a -> IO ()
shouldBeJust (Just x) = return ()
shouldBeJust Nothing = do
  error ("error: shouldBeJust: Nothing")
  

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
  return $! BSL.length contents
  BSL.writeFile path contents'

