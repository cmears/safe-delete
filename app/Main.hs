{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Options.Applicative

import Witness

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optionsParser) (progDesc "safe-delete"))
  forM_ (optionTargets opts) $ \target -> do
    maybeWitness <- findWitness target (optionBaseDir opts)
    case maybeWitness of
      Nothing -> putStrLn ("no witness for " ++ target)
      Just w -> putStrLn (w ++ " is witness for " ++ target)
  return ()

data Options = Options {
    optionBaseDir :: FilePath
  , optionDryRun :: Bool
  , optionTargets :: [FilePath]
} deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options <$> strOption (long "base" <> metavar "DIR" <> value "." <>
                help "Base directory to search for witnesses")
          <*> switch (long "dry-run" <> short 'n' <>
                help "Don't delete any files")
          <*> many (strArgument (metavar "FILE ..."))
