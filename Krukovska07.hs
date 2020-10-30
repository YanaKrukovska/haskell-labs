{-# OPTIONS_GHC -Wall #-}
module Krukovska07 where

import Data.List

data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq) 
-- B-tree t-way (NodeB kl tl) =>  
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a =  NodeB [a] [Btree a]  deriving (Show, Eq)

-- main characteristics of B-tree  (BInform heigth min max)
data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)

-- Task 1 ------------------------------------
compareValues :: (Ord a) => a -> BinTreeM a -> BinTreeM a -> Bool
compareValues _ (EmptyM) (EmptyM) = True
compareValues v (EmptyM) (NodeM rightValue _ _ _) = v < rightValue
compareValues v (NodeM leftValue _ _ _) (EmptyM) = v > leftValue
compareValues v (NodeM leftValue _ _ _) (NodeM rightValue _ _ _) = v > leftValue && v < rightValue

isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch (NodeM v k tl tr) = if (k <= 0) then False else compareValues v tl tr && isSearch tl && isSearch tr

-- Task 2 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch (NodeM nodeV _ tl tr) v | nodeV == v = True
                                   | v > nodeV = elemSearch tr v
                                   | otherwise = elemSearch tl v
elemSearch (EmptyM) _ = False     

-- Task 3 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch EmptyM v = NodeM v 1 EmptyM EmptyM
insSearch (NodeM nodeV k tl tr) v | v == nodeV = NodeM nodeV (k + 1) tl tr
                                  | v < nodeV = NodeM nodeV k (insSearch tl v) tr
                                  | otherwise = NodeM nodeV k tl (insSearch tr v)

-- Task 4 ------------------------------------
delSearchHelper :: (Ord a) => BinTreeM a -> BinTreeM a -> BinTreeM a
delSearchHelper tl EmptyM = tl
delSearchHelper EmptyM tr = tr
delSearchHelper (NodeM v k tl tr) newTree = NodeM v k tl(delSearchHelper tr newTree)

delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
delSearch EmptyM _ = EmptyM
delSearch (NodeM nodeV k tl tr) v | compareResult == LT = NodeM nodeV k (delSearch tl v) tr
                                  | compareResult == GT = NodeM nodeV k tl (delSearch tr v)
                                  | otherwise = if k == 1 then delSearchHelper tl tr else (NodeM nodeV (k-1) tl tr)
                                    where compareResult = compare v nodeV

-- Task 5 ------------------------------------
sortListHelper :: BinTreeM a -> [a]
sortListHelper EmptyM = []
sortListHelper (NodeM v k tl tr) = sortListHelper tl ++ replicate k v ++ sortListHelper tr

buildTree::(Ord a) => [a] -> BinTreeM a
buildTree = foldl insSearch EmptyM

sortList :: (Ord a) => [a] -> [a]
sortList ls = sortListHelper (buildTree ls)

-- Task 6 ------------------------------------
findHeight :: Btree a -> Int
findHeight (NodeB _ []) = 0
findHeight (NodeB _ trs) = 1 + (findHeight (head trs))

findMin :: Btree a -> a
findMin (NodeB (k:_) []) = k
findMin (NodeB _ trs) = findMin (head trs)

findMax :: Btree a -> a
findMax (NodeB ks []) = last ks
findMax (NodeB _ trs) = findMax (last trs)

findBInform :: (Bounded a, Ord a) => Btree a ->  BInform a
findBInform (NodeB ks []) = BInform 0 (head ks) (last ks)
findBInform tr = BInform (findHeight tr) (findMin tr) (findMax tr)

-- Task 7 ------------------------------------
isBtree :: (Bounded a, Ord a) => Int -> Btree a -> Bool 
isBtree = undefined

-- Task 8 ------------------------------------
bTreeIntoList :: (Ord a) => Btree a -> [a]
bTreeIntoList (NodeB v tr) = if (length tr == 0) then v else bTreeIntoList (NodeB v (tail tr)) ++ bTreeIntoList (head tr)

eqBtree :: (Bounded a, Ord a) => Int -> Btree a -> Btree a -> Bool 
eqBtree _ tr1 tr2 = sort (bTreeIntoList tr1) == sort (bTreeIntoList tr2)

-- Task 9 ------------------------------------
elemBtree :: Ord a => Btree a -> a -> Bool
elemBtree (NodeB [] _) _= False
elemBtree (NodeB nodeV []) v = elem v nodeV
elemBtree (NodeB nodeV (t:tr)) v = elemBtree t v || elemBtree (NodeB nodeV tr) v

position :: Ord a => a -> [a] -> Int
position _ [] = 0
position v (x:xs) = if (v == x || v < x) then 0 else 1 + position v xs

-- Task 10 ------------------------------------
insBtree :: Ord a => Int -> Btree a -> a -> Btree a
insBtree t tr v | (isFull t tr) = let (l, k, b) = splitAtB t tr in insertInNode t (NodeB [k] [l, b]) v
                | otherwise = insertInNode t tr v

isFull :: Ord a => Int -> Btree a -> Bool
isFull size (NodeB v _) = 2 * size - 1 == length v

insertKey :: Ord a => a -> [a] -> [a]
insertKey v (x:xs)| v < x = [x] ++ (insertKey v xs)
                  | v == x = [v, v] ++ xs
                  | otherwise = [v, x] ++ xs
insertKey v [] = [v]

decomposeNodeB :: Ord a => a -> [a] -> [Btree a] -> ([a], [a], [Btree a], Btree a, [Btree a])
decomposeNodeB v kl tl = let index = position v kl
                             kl1 = [kl !! ks | ks <- [0..(index - 1)]]
                             kl2 = [kl !! ks | ks <- [index..(length kl) - 1]]
                             tl1 = [tl !! ts | ts <- [0..(index - 1)]]
                             tl2 = [tl !! ts | ts <- [index + 1..(length tl) - 1]]
                             bt = tl !! index
                         in (kl1, kl2, tl1, bt, tl2)

splitAtB :: Ord a => Int -> Btree a -> (Btree a, a, Btree a)
splitAtB t (NodeB kl tl) = let kl1 = [kl !! ks | ks <- [0..t - 2]]
                               kl2 = [kl !! ks | ks <- [t..2 * t - 2]]
                               tl1 = if (tl == []) then [] else [tl !! ts | ts <- [0..t - 1]]
                               tl2 = if (tl == []) then [] else [tl !! ts | ts <- [t..2 * t - 1]]
                           in (NodeB kl1 tl1, kl !! (t - 1), NodeB kl2 tl2)

insertInNode :: Ord a => Int -> Btree a -> a -> Btree a
insertInNode _ (NodeB ks []) v = NodeB (insertKey v ks) []
insertInNode t (NodeB ks ts) v = if (isFull t bt) then
                                    let (bt1, k, bt2) = splitAtB t bt
                                        tree1 = if v <= k then (insertInNode t bt1 v) else bt1
                                        tree2 = if v <= k then bt2 else (insertInNode t bt2 v)
                                    in NodeB (kl1 ++ (k : kl2)) (tl1 ++ (tree1 : (tree2 : tl2)))
                                 else NodeB ks (tl1 ++ ((insertInNode t bt v):tl2))
                                 where (kl1, kl2, tl1, bt, tl2) = decomposeNodeB v ks ts

--------------------- Test Data - Search Trees -------
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   

tBt1 :: Btree Char 
tBt1 = NodeB "L"
       [ NodeB "DG" 
          [ NodeB "AC" [], NodeB "EE" [], NodeB "HK" []
          ]
       , NodeB "PU" 
          [ NodeB "MM" [], NodeB "RS" [], NodeB "UW" []
          ]
       ]

tBt2 :: Btree Char 
tBt2 = NodeB "GP"
       [ NodeB "ACDEE" [], NodeB "HKLMM" [], NodeB "RSUUW" []
       ]

tBt5 :: Btree Char 
tBt5 = NodeB "GMPX"
       [ NodeB "ACDE" [] , NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt6 :: Btree Char 
tBt6 = NodeB "GMPX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt7 :: Btree Char 
tBt7 = NodeB "GMPTX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
       ]

tBt8 :: Btree Char 
tBt8 = NodeB "P"
       [ NodeB "GM"
          [ NodeB "ABCDE" [], NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]

tBt9 :: Btree Char 
tBt9 = NodeB "P"
       [ NodeB "CGM"
          [ NodeB "AB" [], NodeB "DEF" []
          , NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]
