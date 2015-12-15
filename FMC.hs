module FMC
(
  genClusterCenters,
  genDegreeOfMembership,
  genVTable,
  genUTable,
  fmc1,
  fmc2
) where

import Data.List
import System.Random
import Help

  -----Table V (cluster centers) size: [l x m], where l = clusters, m = object parameters

sumVectors :: Num a => [[a]] -> [a]
sumVectors = foldl1 (\acc b -> zipWith (+) b acc) 

effect :: Num a => [a] -> a -> [a] 
effect x m = map ( * (m^2)) x

genClusterCenters :: Floating a => [[a]] -> [a]-> [a]
genClusterCenters x m = map ( * (1 / (sum $ map (^2) m))) (sumVectors (zipWith (effect) x m))

genVTable :: Floating a => [[a]] -> [[a]] -> [[a]]
genVTable u x = foldr (\m acc -> (genClusterCenters x m) : acc) [] (transpose u)  

-----Table U (objects degree of membership) size: [n x l], where n - objects, l - clusters

distTable :: Floating a => ([a] -> [a] -> a) -> [a] -> [[a]] -> [a] 
distTable f x v = map (f x) v

genDegreeOfMembership :: Floating a => [a] -> [a]
genDegreeOfMembership table  = map (degree table) table
  where degree t c = 1 / (foldl (\acc x -> acc + (c / x)^2 ) 0 t)

genUTable :: Floating a => ([a] -> [a] -> a) -> [[a]] -> [[a]] -> [[a]]
genUTable f v x = foldr (\i acc -> (genDegreeOfMembership (distTable f i v)) : acc) [] x 

--------------------------------------------------------------------------------------------
----------  Algorithm

findMin :: Ord a => [[a]] -> a
findMin = minimum . map (minimum)

findMax :: Ord a => [[a]] -> a
findMax = maximum . map (maximum)

fmc :: (Ord a, Floating a) => ([a] -> [a] -> a) -> [[a]] -> [[a]] -> [[a]] -> a -> [[a]]
fmc f x u v e = let 
    newV = genVTable u x 
    newU = genUTable f newV x
    in if (matrixNorm newU u) > e then (fmc f x newU newV e) else newU

fmc1 :: RandomGen g => g -> ([Float] -> [Float] -> Float) -> [[Float]] -> Float -> Int -> [[Float]]
fmc1 gen f x e cNum = let
    newU = normalizeMatrix $ genRandomTable gen (length x) cNum
    in fmc f x newU [] e

fmc2 :: RandomGen g => g -> ([Float] -> [Float] -> Float) -> [[Float]] -> Float -> Int -> [[Float]]
fmc2 gen f x e cNum = let
    newV = genRandomTableInRange gen cNum (length (x !! 0)) (findMin x) (findMax x)
    newU = genUTable f newV x
    in fmc f x newU newV e



    