{-# OPTIONS_GHC -Wall #-}
module Krukovska05 where

import Data.Char(isUpper)
import Data.List

type Grammar    = [Production]         -- Context free grammar
type Production = (Char,String)        -- Правило виводу
type Predict    = [(Char,String)]      -- Прогнозуюча таблиця
type Control    = [((Char,Char),Int)]  -- Управляюча таблиця 

-- Task 1 ------------------------------------
addOne :: String -> Char -> String  
addOne st c = if (elem c st) then st else sort (st ++ [c])

addAll :: String -> String -> String 
addAll st [] = st
addAll st (w:wd) = addAll (addOne st w) wd

addWithout :: String -> String -> String 
addWithout st wd = addAll st (filter (not . (`elem` "$")) wd)

inter :: String -> String -> String 
inter st1 st2 = filter (\s -> elem s st2) st1 

-- Task 2 ------------------------------------
tkPredict :: Predict -> Char -> String 
tkPredict pt n | null pt = "" 
               | (fst (head pt)) == n = snd (head pt) 
               | otherwise = tkPredict (tail pt) n

upPredict :: Predict -> Char -> String -> Predict 
upPredict [] n st = [(n, st)]
upPredict (p:pt) n st 
    | (fst p == n) = (n, st):pt
    | (fst p > n) = (n, st):p:pt
    | otherwise = p:(upPredict pt n st)

-- Task 3 ------------------------------------
parse ::  Grammar -> Control -> String -> Maybe [Int]
parse = undefined

step :: Grammar -> Control -> 
       (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
step = undefined

-- Task 4 ------------------------------------
first :: Predict -> String -> String
first _ [] = ['$']
first pFst (s:st) | isUpper s = if elem '$' terminals then addWithout (first pFst st) terminals 
                                else terminals | (s == '$') = first pFst st
                                               | otherwise = [s] where terminals = tkPredict pFst s 

-- Task 5 ------------------------------------
addResult :: Char -> String -> Predict -> Int -> [((Char, Char), Int)]
addResult ch str pr n | fst (head pr) == ch = [((ch, s), n) | s <- str]
                      | otherwise = addResult ch str (tail pr) n

buildHelper :: Grammar -> Predict -> Predict -> Int -> Control
buildHelper gr pFst pNxt n  | null gr = []
                            | predictChar == "$" = table ++ addResult (fst (head gr)) (tkPredict pNxt (fst (head gr))) pNxt n
                            | predictChar /= "$" = table ++ addResult (fst (head gr)) predictChar pFst n
                            | otherwise = table
                            where predictChar = first pFst (snd (head gr))
                                  table = buildHelper (tail gr) pFst pNxt (n + 1)

sortControl :: Control -> Control
sortControl controlToSort = sortBy (\(a, _) (b, _) -> compare a b) controlToSort

buildingControl :: Grammar -> Predict -> Predict -> Control 
buildingControl gr pFst pNxt = sortControl (buildHelper gr pFst pNxt 0)


-- Task 6 ------------------------------------
testingLL1 :: Grammar -> Predict -> Predict -> Bool
testingLL1 = undefined

fromGrammar :: Grammar ->  [(Char,[String])]
fromGrammar = undefined

testFst :: [String] -> Bool
testFst = undefined

testFollow :: String -> [String] -> Bool
testFollow = undefined

-- Task 7 ------------------------------------
buildFstHelper :: Grammar -> Predict -> Predict
buildFstHelper gr pFst | pFst == (evalFst gr pFst) = pFst
                       | otherwise = buildFstHelper gr (evalFst gr pFst) 

initPredictFst :: Grammar -> Predict -> Predict 
initPredictFst (g:gr) predict |null (snd(g)) = initPredictFst gr (upPredict predict (fst g) "$")
                            | otherwise = initPredictFst gr (upPredict predict (fst g) (addAll (tkPredict predict (fst g)) ""))
initPredictFst _ predict = predict

buildFst :: Grammar -> Predict 
buildFst gr = buildFstHelper gr (initPredictFst gr [])

evalFst :: Grammar -> Predict -> Predict
evalFst (g:gr) pFst = evalFst gr (extandFst pFst g)
evalFst _ pFst = pFst

extandFst :: Predict -> Production -> Predict 
extandFst pFst (n, rul) = upPredict pFst n (addAll (first pFst (rul)) (tkPredict pFst n))

-- Task 8 ------------------------------------
buildNxt :: Grammar -> Predict -> Predict 
buildNxt = undefined

nontermTails :: Grammar -> [(Char,String)] 
nontermTails = undefined

evalNxt :: [(Char,String)] -> Predict -> Predict -> Predict
evalNxt = undefined

extandNxtOne :: Predict -> Char -> Predict -> String -> Predict
extandNxtOne = undefined

---------------------Test data---------------------------
 
gr0, gr1, gr2, gr3, gr4, gr5:: Grammar

-- LL(1)-grammar
gr0 = [('S',"aAS"),('S',"b"), ('A',"a"), ('A',"bSA")]  
gr1 = [('S',"TV"),('T',"d"),('T',"(S)"),('V',"+TV"),('V',"-TV"),('V',"")]  
gr2 = [('E',"TU"),('U',""),('U',"+TU"),('U',"-TU"),
       ('T',"FV"),('V',""),('V',"*FV"),('V',"%FV"),('V',"/FV"),
       ('F',"d"),('F',"(E)")] 

-- not LL(1)-grammar
gr3 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba"),('S',"")]
gr4 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   
gr5 = [('E',"E+T"), ('E',"E-T"),('E',"T"), 
       ('T',"T*F"), ('T',"T%F"), ('T',"T/F"), ('T',"F"), 
       ('F',"d"),('F',"(E)") ]

-- predictive tables for starting terminals Fst
pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A',"ab"),('S',"ab")]
pFst1 = [('S',"(d"),('T',"(d"),('V',"$+-")]
pFst2 = [('E',"(d"),('F',"(d"),('T',"(d"),('U',"$+-"),('V',"$%*/")]
pFst3 = [('A',"ab"),('S',"$a")]
pFst4 = [('E',"(d"),('F',"(d"),('T',"(d")]
pFst5 = [('E',"(d"),('F',"(d"),('T',"(d")]

-- predictive tables for next terminals Nxt
pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A',"ab"),('S',"$ab")]
pNxt1 = [('S',"$)"),('T',"$)+-"),('V',"$)")]
pNxt2 = [('E',"$)"),('F',"$%)*+-/"),('T',"$)+-"),('U',"$)"),('V',"$)+-")]
pNxt3 = [('A',"$ab"),('S',"$b")]
pNxt4 = [('E',"$)+"),('F',"$)*+"),('T',"$)*+")]
pNxt5 = [('E',"$)+-"),('F',"$%)*+-/"),('T',"$%)*+-/")]   

-- control tables
ctl0, ctl1, ctl2 :: Control 
ctl0 = [(('A','a'),2),(('A','b'),3),(('S','a'),0),(('S','b'),1)]
ctl1 = [(('S','('),0),(('S','d'),0),(('T','('),2),(('T','d'),1),
        (('V','$'),5),(('V',')'),5),(('V','+'),3),(('V','-'),4)]
ctl2 = [(('E','('),0),(('E','d'),0),(('F','('),10),(('F','d'),9),
        (('T','('),4),(('T','d'),4),(('U','$'),1),(('U',')'),1),
        (('U','+'),2),(('U','-'),3),(('V','$'),5),(('V','%'),7),
        (('V',')'),5),(('V','*'),6),(('V','+'),5),(('V','-'),5),(('V','/'),8)]

