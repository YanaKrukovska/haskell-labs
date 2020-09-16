{-# OPTIONS_GHC -Wall #-}
module Krukovska02 where

-- Task 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl  = foldl (+) 0;
  
-- Task 2 ----------------------------------------- 
productFr :: [Integer] -> Integer
productFr  = foldr (*) 1;

-- Task 3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (\x y -> x:y) ys xs

-- Task 4 -----------------------------------------
insert :: [Int] -> Int -> [Int]
insert xs v = [y | y <- xs, y <= v] ++ [v] ++ [y | y <- xs, y > v]

sortInsert :: [Int] -> [Int]
sortInsert  = foldl insert []

-- Task 5 -----------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices p xs = map fst(filter(p.snd)(zip [0..] xs))

-- Task 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse xss = reverse (map reverse xss)

-- Task 7  -----------------------------------------
noDigits :: String -> String
noDigits xs = filter (\x -> not (elem x ['0'..'9'])) xs

-- Task 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood ps v = length [n | n <- ps, n v]

-- Task 9 ------------------------------------------
calculateNextRow :: [Integer] -> [Integer]
calculateNextRow xs = zipWith (+) (0:xs) (xs ++ [0])

trianglePas :: [[Integer]]
trianglePas = iterate calculateNextRow [1]

-- Task 10 -----------------------------------------
factorialsM :: [Integer]
factorialsM = zipWith (*) [1..] (1:factorialsM)
