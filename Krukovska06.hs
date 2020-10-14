{-# OPTIONS_GHC -Wall #-}
module Krukovska06 where

import Data.List(nub)

type GraphS = (Int,[(Int,Int)])            
type Graph  = [[Int]]

--------------------- Test data - Graphs -------
gr1S, gr2S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (7,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])

gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]

-- Task 1 ------------------------------------
isOrdinary :: Graph -> Bool 
isOrdinary gr = notElem False [x `elem` gr !! y | x <- nodes gr, y <- gr !! x]

-- Task 2 ------------------------------------
fromGraph :: Graph -> GraphS 
fromGraph gr = (length gr - 1, edges gr)

-- Task 3 ------------------------------------
toGraph :: GraphS -> Graph
toGraph (amount, graph) = [map snd y | y <- [filter (\n -> fst n == x) graph | x <- [0 .. amount]]]

-- Task 4 ------------------------------------
shortWay :: Graph -> Int -> Int -> [Int]
shortWay gr a b | length res == 0 = []
                | otherwise = head res
                 where res = shortWays gr a b

-- Task 5 ------------------------------------
isConnecting :: Graph -> Bool 
isConnecting gr = length gr == (length $ goNodes gr 0)

-- Task 6 ------------------------------------
components :: Graph -> [[Int]] 
components = undefined

-- Task 7 ------------------------------------
eccentricity :: Graph -> Int -> Int 
eccentricity = undefined

-- Task 8 ------------------------------------
findDiameter :: Graph -> Int 
findDiameter = undefined

findRadius :: Graph -> Int 
findRadius = undefined

-- Task 9 ------------------------------------
findCenter :: Graph -> [Int] 
findCenter = undefined

-- Task 10 ------------------------------------
findWays :: Graph -> Int ->Int -> [[Int]]
findWays gr a b = filter (\x -> b == head x) (concat (allWays gr a))

shortWays :: Graph -> Int -> Int -> [[Int]] 
shortWays gr a b | length res == 0 = []
                 | otherwise = map (reverse) (filter (\x-> (length (last res)) == (length x)) res) 
                 where res = findWays gr a b

--------------------- Lecture functions -------

adj :: Graph -> Int -> [Int] 
adj g v = g !! v

nodes :: Graph -> [Int]
nodes g = [0..(length g - 1)]

edgeIn :: Graph -> (Int, Int) -> Bool
edgeIn g (x,y) = elem y (g!!x)

edges :: Graph -> [(Int,Int)]
edges g = [(x, y) | x <- nodes g, y <- g !! x]  

subsets :: [Int] -> [[Int]]
subsets [] = [[]]
subsets (x:xs) = let xss = subsets xs in map (x: ) xss  ++ xss 

allEdges :: [Int] -> [(Int,Int)]
allEdges xs = [(x,y) | x <- xs, y <- xs, x /= y]

isClique :: Graph -> [Int] -> Bool
isClique gr xs = let es = allEdges xs in  null $ filter (not . (edgeIn gr)) es 

cliqueNum :: Graph -> Int
cliqueNum gr = let  xss = subsets (nodes gr)
                    qs = filter  (isClique gr) xss in   maximum $ map length qs

allWays :: Graph -> Int -> [[[Int]]]
allWays gr v = until condW (stepW gr) [[[v]]]

condW :: ([[[Int]]]) -> Bool
condW wss = null ( head wss)   

stepW :: Graph -> [[[Int]]] -> [[[Int]]]
stepW gr wss@(wsn:_) = [t:w | w@(x:xs) <- wsn, notElem x xs, t<- gr!!x] : wss
stepW _ [] = []

goNodes :: Graph -> Int -> [Int]
goNodes gr v = snd $ until cond (oneStep gr) ([v],[])

cond :: ([Int],[Int]) -> Bool
cond (ns,_) = ns == []

oneStep :: Graph -> ([Int], [Int]) -> ([Int],[Int]) 
oneStep gr (ns,os) = let old = ns ++os
                         ns1 = concatMap (gr !!) ns
                         ns2 = filter (`notElem` old) ns1
                         new = nub ns2
 in (new,old)
