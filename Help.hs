module Help
( 
  hammingDist,
  euclideanDist,
  matrixNorm,
  genRandomTable,
  genRandomTableInRange,
  normalizeMatrix
) where

import System.Random
import Data.List

--------Distance-----------------

hammingDist :: Floating a => [a] -> [a] -> a
hammingDist x y = sum . map (abs) $ zipWith (-) x y

euclideanDist :: Floating a => [a] -> [a] -> a
euclideanDist x y = sqrt . sum . map (^2) $ zipWith (-) x y

--------Matrix Routine--------------

matrixSub :: Num a => [[a]] -> [[a]] -> [[a]]
matrixSub x y = zipWith (zipWith (\b c -> abs(b - c))) x y 

matrixNorm :: (Num a, Ord a) => [[a]] -> [[a]] -> a
matrixNorm x y = maximum $ map (maximum) (matrixSub x y)

normalizeVector :: Floating a => [a] -> [a]
normalizeVector v = let summ = sum v in map (\x -> x/summ) v

normalizeMatrix :: Floating a => [[a]] -> [[a]]
normalizeMatrix = map (normalizeVector)

--------Random------------------

genTable :: [Float] -> Int -> Int -> [[Float]]
genTable _ 0 _ = []
genTable rlist h w = let (first, rest) = splitAt w rlist 
 in first : genTable rest (h - 1) w

genRandomTable :: RandomGen g => g -> Int -> Int -> [[Float]]
genRandomTable gen h w = let 
  randomNum = take (h*w) (randomRs (0,100000) gen :: [Float])
  in genTable randomNum h w

genRandomTableInRange :: RandomGen g => g -> Int -> Int -> Float -> Float -> [[Float]]
genRandomTableInRange gen h w from to = let 
  randomNum = randomRs (from,to) gen :: [Float]
  in genTable randomNum h w
