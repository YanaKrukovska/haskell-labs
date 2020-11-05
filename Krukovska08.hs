{-# OPTIONS_GHC -Wall #-}
module Krukovska08 where

import Data.List(find)

data Recur = Zero | Succ | Sel Int Int 
           | Super Recur [Recur] 
           | Prim Recur Recur 
           | Mini Recur Int 
           | Name String  deriving (Show, Eq)
type System = [(String,Recur)]  

-- Task 1 ------------------------------------
findFunction :: System -> String -> Recur
findFunction syst name = (snd . head) result where result = filter (( == name). fst) syst

isNumbConst :: System -> Recur -> Bool 
isNumbConst _ Zero = True
isNumbConst _ (Sel _ _) = True
isNumbConst syst (Super Succ f) = isNumbConst syst (head f) && evRank syst (Super Succ f) == 1
isNumbConst syst(Super Zero f) = isNumbConst syst (head f) && evRank syst (Super Succ f) == 1
isNumbConst syst (Name name) = isNumbConst syst (findFunction syst name)
isNumbConst _ _ = False

-- Task 2 ------------------------------------
evRank :: System -> Recur -> Int 
evRank _ Zero = 1
evRank _ Succ = 1
evRank _ (Sel n _) = n
evRank syst (Super _ al) = evRank syst (head al)
evRank syst (Prim _ st) = (evRank syst st) - 1
evRank syst (Mini b _) = (evRank syst b) - 1
evRank syst (Name functionName) = evRank syst (findFunction syst functionName)

-- Task 3 ------------------------------------
getAllNames :: Recur -> [String]
getAllNames (Name name) = [name]
getAllNames (Super b al) = (getAllNames b) ++ (concatMap (getAllNames)al)
getAllNames (Prim i st) = (getAllNames i) ++ (getAllNames st)
getAllNames (Mini b _) = getAllNames b
getAllNames _ = []

isNamesHelper :: System -> [String] -> Bool
isNamesHelper (s:syst) names | and (map (\x -> elem x names)(getAllNames (snd s))) = isNamesHelper syst (names ++ [(fst s)])
                             | otherwise = False
isNamesHelper [] _ = True

isNames :: System -> Bool 
isNames syst = isNamesHelper syst []

-- Task 4 ------------------------------------
isCorrect :: System -> Recur -> Bool
isCorrect syst f | length syst == 0 = null (getAllNames f)
                 | otherwise = null (filter (\x -> notElem x (map fst syst)) (getAllNames f))

isUsedRight :: System -> Recur -> Bool
isUsedRight syst f | length syst == 0 = isCorrect [] f
                   | isCorrect syst f = isUsedRight (init syst) (snd $ last syst)
                   | otherwise = False

isRecur :: System -> Recur -> Bool
isRecur syst (Name name) = elem name (map fst syst)
isRecur syst f = (elem f (map snd syst)) || isUsedRight syst f

-- Task 5 ------------------------------------
getFuctionByName :: System -> String -> Maybe (String,Recur)
getFuctionByName syst name = find (\x -> name == (fst x)) syst

eval :: System -> Recur -> [Int] -> Int
eval _ (Zero) _ = 0
eval _ (Succ) v = (head v) + 1
eval _ (Sel _ k) v = v !! (k - 1)
eval syst(Super f fs) v = eval syst f (map (\x -> eval syst x v) fs)
eval syst (Name str) xs = case findRecurion syst str of 
                           Just recursion -> eval syst recursion xs  
                           Nothing -> 0
eval syst (Prim r1 r2) xs = last (fst (until cond1 (stepEval syst r2) (init xs ++ [0] ++ [eval syst r1 (take (evRank syst r1) xs)], last xs)))
eval syst minRec@(Mini _ _) xs = case evalPart syst minRec xs of 
    Just res -> res
    Nothing -> 0
cond1::([Int],Int)->Bool
cond1 (xs,i)= i <= (xs !! (length xs -2))

stepEval::System -> Recur -> ([Int],Int) -> ([Int],Int)
stepEval syst r1 (xs,i)= let without1 = init xs
                             counter = (last without1)
                             without2 = init without1
                         in (without2 ++ [counter+1]++[eval syst r1 (without2++[counter]++[last xs])],i)

findRecurion :: System -> String -> Maybe Recur
findRecurion (x:xs) str | fst x == str = Just (snd x)
                        | otherwise = findRecurion xs str 
findRecurion [] _ = Nothing

-- Task 6 ------------------------------------
evalPart :: System -> Recur -> [Int] -> Maybe Int 
evalPart = undefined

-- Task 7 ------------------------------------
parseRec :: String -> Maybe System 
parseRec = undefined


--------------------- Test Data --------
syst1, syst2 :: System 
syst1 = [("const0", Zero)   
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]]) 
   , ("const2", Super Succ [Super Succ [Zero]]) 
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ])) 
   , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))  
   , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))  
   , ("subtract1", Prim Zero (Sel 2 1))  
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))  
   , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])     
   , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])  
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])  
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)    
   ]
   
syst2 = [("f1", Super Succ [Zero])
        ,("f2", Super Succ [Name "f2"])
        ]


sysStr1,sysStr2 :: String    
sysStr1 = " const0 = z1; const0v2  = (z1 : s21); const0v3 = (z1:s31);\n\
          \  const1v2 = (a1 : (z1 : s21));  \n\
          \  const2= (a1:(a1:z1)); addition = [s11, (a1:s33)] ;\n\
          \  multiplication = [z1 , (addition: s33,s31)]; \n\
	  \  notSignum = [(a1:z1),(z1:s21)];\n\
	  \  subtract1 = [z1,s21]; subtraction = [s11, (subtract1:s33)];\n\
	  \  subtractionRev = (subtraction : s22, s21);\n\
          \  subtractionAbs = (addition: subtraction, subtractionRev); \n\
          \  subtractionAbs3=(subtractionAbs:s31, (addition:s32,s33))  ;\n \
          \ subtractionPart = {subtractionAbs3, 100 };"
 
sysStr2 = " f1 = (a1:z1); f2 = (a1, f2);"
