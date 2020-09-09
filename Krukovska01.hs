{-# OPTIONS_GHC -Wall #-}
module Krukovska01 where

-- Task 1 -----------------------------------------
power3 :: [Integer]
power3 = [x^(3::Integer) | x <- [1..]]  

-- Task 2 -----------------------------------------
toPower3 :: [Integer]
toPower3 = [(3::Integer)^x | x <- [(1::Integer)..]]

-- Task 3 -----------------------------------------
sumPower3 :: Integer -> Integer
sumPower3 n = sum[(3::Integer)^x | x <- [1..n]]

-- Task 4 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower m n = sum [m^i |(m >= 0), i <- [1 .. n]]

-- Task 5 -----------------------------------------
lessMe :: [Int] -> [Int]
lessMe [] = []
lessMe xs = map (\x -> (length . filter (< x)) xs) xs

-- Task 6 -----------------------------------------
frequency :: [Int] -> [(Int,Int)]
frequency [] = []
frequency (x:xs) = (x, (length (filter (\n -> n == x) xs)) + 1):frequency (filter (\n -> n /= x) xs)

-- Task 7 -----------------------------------------
hailstone :: Int -> Int
hailstone n = if mod n 2 == 0 then div n 2 else 3 * n + 1

-- Task 8 -----------------------------------------
hailSeq :: Int -> [Int]
hailSeq n = if (n == 0) then [0] else if (n == 1) then [1] else n:hailSeq (hailstone n)

-- Task 9 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = [hailSeq x | x <- [1..]]

-- Задача 10 -----------------------------------------
firstHailSeq :: Int -> Int
firstHailSeq l = head [x | x <- [1 ..], (length (hailSeq x) == l)]