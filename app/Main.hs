{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Options.Applicative

import Witness

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optionsParser) (progDesc "safe-delete"))
  expandedTargets <- concat <$> mapM expandTarget (optionTargets opts)

  let numTargets = length expandedTargets
  putStrLn ("checking " ++ show numTargets ++ " files...")

  let doTarget tally target = do
        maybeWitness <- findWitness (optionFastMatch opts) target (optionBaseDir opts)
        case maybeWitness of
          Nothing -> do putStrLn ("no witness for " ++ target)
                        return tally
          Just _ -> return $! tally + 1

  finalTally <- foldM doTarget 0 expandedTargets

  putStrLn ("checked " ++ show numTargets ++ " files")
  putStrLn ("found " ++ show finalTally ++ " witnesses")
  putStrLn ("found " ++ show (numTargets - finalTally) ++ " unwitnessed files")  

data Options = Options {
    optionBaseDir :: FilePath
  , optionDryRun :: Bool
  , optionFastMatch :: Bool
  , optionTargets :: [FilePath]
} deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options <$> strOption (long "base" <> metavar "DIR" <> value "." <>
                help "Base directory to search for witnesses")
          <*> switch (long "dry-run" <> short 'n' <>
                help "Don't delete any files")
          <*> switch (long "fast-match" <> short 'f' <>
                help "Only match file names and sizes, not contents")
          <*> many (strArgument (metavar "FILE ..."))
