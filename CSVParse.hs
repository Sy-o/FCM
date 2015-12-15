module CSVParse
(
    handlerForFileRoutine,
    getMaybeData
)where

import System.IO.Error
import Data.List.Split.Internals
import Data.Maybe

handlerForFileRoutine name e
  | isDoesNotExistError e = error ("File "++ name ++" does not exist") 
  | isAlreadyInUseError e = error ("File " ++ name ++" is already used ")
  | isPermissionError e = error   ("Permission error while trying open file "++ name) 
  | otherwise = ioError e 

getStringMatrix :: String -> String -> [[String]]
getStringMatrix source splitter = map (splitOn splitter) $ splitOn "\n" source

readMaybe :: String -> Maybe Float
readMaybe s = case reads s of
    [(x, _)] -> Just x
    _        -> Nothing

getMaybeData source splitter t fc lc = getMaybeMatrix $ cutM (getStringMatrix source splitter) t fc lc

cutM matrix title fstc lstc = cut lstc (map init) (cut fstc (map tail)  (cut title tail matrix))
  where cut b f x = if b then f x else x

getMaybeVector :: [String] -> [Maybe Float]
getMaybeVector = map readMaybe

getMaybeMatrix :: [[String]] -> Maybe [[Float]] 
getMaybeMatrix m = unpackMabeMatrix $ map getMaybeVector m

unpackMabeVector :: [Maybe a] -> Maybe [a]
unpackMabeVector v = mV [] v
   where mV acm [] = Just (reverse acm)
         mV acm ((Just a):xs) = mV (a:acm) xs
         mV acm (Nothing:xs) = Nothing

unpackMabeMatrix :: [[Maybe a]] -> Maybe [[a]]
unpackMabeMatrix m = unpackMabeVector $ map unpackMabeVector m

