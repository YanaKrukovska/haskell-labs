{-# OPTIONS_GHC -Wall #-}
module Krukovska08 where

data Recur = Zero | Succ | Sel Int Int 
           | Super Recur [Recur] 
           | Prim Recur Recur 
           | Mini Recur Int 
           | Name String  deriving (Show, Eq)
type System = [(String,Recur)]  

-- Task 1 ------------------------------------
isNumbConst :: System -> Recur -> Bool 
isNumbConst _ Zero = True
isNumbConst _ (Sel _ _) = True
isNumbConst syst (Super Succ f) = isNumbConst syst (head f) && evRank syst (Super Succ f) == 1
isNumbConst syst(Super Zero f) = isNumbConst syst (head f) && evRank syst (Super Succ f) == 1
isNumbConst syst (Name name) = isNumbConst syst (findFunction syst name)
isNumbConst _ _ = False

findFunction :: System -> String -> Recur
findFunction syst name = (snd . head) result where result = filter (( == name). fst) syst

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
isNames :: System -> Bool 
isNames = undefined

-- Task 4 ------------------------------------
isRecur :: System -> Recur -> Bool
isRecur = undefined

-- Task 5 ------------------------------------
eval :: System -> Recur -> [Int] -> Int 
eval = undefined

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
