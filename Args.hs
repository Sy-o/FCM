{-# LANGUAGE DeriveDataTypeable #-}

module Args
(
    parseArgs,
    InputOptions(..)
) where

import Data.List.Split.Internals
import Data.String
import Data.Typeable
import Data.Data

--parseArgs :: [String] -> [[String]]
parseArgs args = map (splitOn " ") args

data InputOptions = InputOptions{
  accuracy :: Float,
  sourceFile :: String,
  numberOfClasses :: Int,
  resultFile :: String,
  metric :: String,
  columnSplitter :: String,
  ignoreTitle :: Bool,
  ignoreFirstCol :: Bool,
  ignoreLastCol :: Bool,
  table :: String     
} deriving (Show, Data, Typeable)