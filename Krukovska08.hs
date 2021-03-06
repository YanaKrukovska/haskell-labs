{-# OPTIONS_GHC -Wall #-}
module Krukovska08 where

import Data.List()
import Text.ParserCombinators.Parsec

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
findRecurion :: System -> String -> Maybe Recur
findRecurion (x:xs) str | fst x == str = Just (snd x)
                        | otherwise = findRecurion xs str 
findRecurion [] _ = Nothing

evaluateNext::System -> Recur -> ([Int],Int) -> ([Int],Int)
evaluateNext syst r (xs,i) = let firstOut = init xs
                                 counter = (last firstOut)
                                 secondOut = init firstOut
                             in (secondOut ++ [counter + 1] ++ [eval syst r (secondOut ++ [counter] ++ [last xs])], i)

eval :: System -> Recur -> [Int] -> Int
eval _ (Zero) _ = 0
eval _ (Succ) v = (head v) + 1
eval _ (Sel _ k) v = v !! (k - 1)
eval syst(Super f fs) v = eval syst f (map (\x -> eval syst x v) fs)
eval syst (Name str) xs = case findRecurion syst str of 
                           Just recursion -> eval syst recursion xs  
                           Nothing -> 0
eval syst (Prim recur1 recur2) xs = last (fst (until compareCondition (evaluateNext syst recur2) (init xs ++ [0] ++ [eval syst recur1 (take (evRank syst recur1) xs)], last xs)))
 where compareCondition::([Int], Int) -> Bool
       compareCondition (x, i) = i <= (x !! (length x - 2))
eval syst minRec@(Mini _ _) xs = case evalPart syst minRec xs of 
    Just result -> result
    Nothing -> 0

-- Task 6 ------------------------------------
evalPart :: System -> Recur -> [Int] -> Maybe Int
evalPart syst (Name name) vl = evalPart syst (findFunction syst name) vl
evalPart syst (Mini g t) vl | null result = Nothing
                            | otherwise = Just (head result)
                          where result = filter (\x -> (eval syst g (vl ++ [x])) == 0)[0..t]
evalPart syst f vl = Just (eval syst f vl)

-- Task 7 ------------------------------------                                  
integer :: Parser Int
integer = do ds <- many1 digit
             return $ read ds

iden :: Parser Recur
iden = do l <- letter
          ss <- many (digit <|> letter)
          return (Name (l:ss))
         
idenName :: Parser String
idenName = do l <- letter
              ss <- many (digit <|> letter)
              return (l:ss)

recur :: Parser Recur
recur = do r <- (try base) <|> (try super) <|> (try prim) <|> (try mini)
           return r
  
base :: Parser Recur
base = do b <- (try zero) <|> (try suc) <|> (try sel) <|> iden
          return b
 
zero :: Parser Recur
zero = do _ <- string "z1"
          return Zero

suc ::Parser Recur
suc = do _ <- string "a1"
         return Succ

sel :: Parser Recur
sel = do _ <- char 's'
         n <- digit
         k <- digit
         return (Sel (read [n]) (read [k]))

super :: Parser Recur
super = do _ <- char '('
           r1 <- recur
           _ <- char ':'
           r2 <- recur
           r3 <- many (repeatedSuper)
           _ <- char ')'
           return (Super r1 (r2:r3))

repeatedSuper :: Parser Recur
repeatedSuper = do _ <- char ','
                   r <- recur
                   return r

prim :: Parser Recur
prim = do _ <- char '['
          r1 <- recur
          _ <- char ','
          r2 <- recur
          _ <- char ']'
          return (Prim r1 r2)

mini :: Parser Recur
mini = do _ <- char '{'
          r <- recur
          _ <- char ','
          i <- integer
          _ <- char '}'
          return (Mini r i)

system :: Parser System
system = do s <- many systemHelper
            eof
            return s;
            
systemHelper :: Parser (String, Recur)
systemHelper = do i <- idenName
                  _ <- char '='
                  r <- recur
                  _ <- char ';'
                  return (i,r)

parseRec :: String -> Maybe System
parseRec str = let filteredStr = filter (/= '\t')(filter (/= ' ') (filter (/= '\n') str))
                   in case parse system "" filteredStr of 
                           Left _ -> Nothing
                           Right r -> Just r

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
