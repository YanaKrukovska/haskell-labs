﻿{-# OPTIONS_GHC -Wall #-}
module Krukovska04 where

import Data.Char(isDigit, digitToInt)

-- Task 1 -----------------------------------------
analyseG :: String -> Bool 
analyseG input = case s input of
    Just input1 -> null input1
    Nothing  -> False

s :: String -> Maybe String
s ('a':input) = case s input of
    Just('b':input1) -> case a input1 of
        Just('a':input2) -> Just input2
        _ -> Nothing
    _ -> Nothing

s ('b':input) = Just input
s _ = Nothing


a :: String -> Maybe String
a ('b':'a':input) = case a input of
    Just input1 -> s input1
    _ -> Nothing

a ('a':input) = Just(input)
a _ = Nothing

-- Task 2 ----------------------------------------
balance :: String -> Bool
balance = undefined

b :: String -> Maybe String 
b = undefined

c :: String -> Maybe String 
c = undefined

e :: String -> Maybe String 
e = undefined

-- Task 3 -----------------------------------------
analyseExpr :: String -> Bool 
analyseExpr input = case ae input of
    Just input2 -> null input2
    _ -> False

ae :: String -> Maybe String 
ae input = case af input of
    Just input2 -> aa input2
    Nothing  -> Nothing

aa :: String -> Maybe String 
aa (d:input) | elem d "+-*"= case af input of
    Nothing  -> Nothing
    Just input2 -> aa input2
aa input = Just input

af :: String -> Maybe String 
af ('(':input) = case ae input of
    Just(')':input2) -> Just input2
    _ -> Nothing

af (d:input) | isDigit d = Just input
af _ = Nothing

-- Task 4 -----------------------------------------
evalLeft :: String -> Maybe Int 
evalLeft input = case le input of 
   Just (value, input2)| null input2 -> Just value 
   _ -> Nothing

le :: String -> Maybe (Int, String) 
le input = case lf input of 
     Just (value1, input2) -> la (value1, input2) 
     Nothing -> Nothing 

la :: (Int, String) -> Maybe (Int, String) 
la (value1,(d:input))| elem d "+-*" = case lf input of  
     Just (value2, input2) -> let value | d == '+' = value1 + value2
                                        | d == '*' = value1 * value2
                                        |otherwise = value1 - value2
                                        in la (value,input2)  
     Nothing -> Nothing 
la (value1, input) = Just (value1, input)

lf :: String -> Maybe (Int, String) 
lf ('(':input) = case le input of 
     Just (value,(')':input2)) -> Just (value, input2) 
     _ -> Nothing  

lf (d:input) | isDigit d = Just (digitToInt d, input) 
lf _ = Nothing

-- Task 5 -----------------------------------------
evalRigth :: String -> Maybe Int 
evalRigth input = case re input of 
   Just (value, input2)| null input2 -> Just value 
   _ -> Nothing
   
re :: String -> Maybe (Int, String) 
re input = case rf input of 
     Just (value1, input2) -> ra (value1, input2) 
     Nothing -> Nothing 

ra :: (Int, String) -> Maybe (Int, String) 
ra (value1,(d:input))| elem d "+-*" = case re input of  
     Just (value2, input2) -> let value | d == '+' = value1 + value2
                                        | d == '*' = value1 * value2
                                        |otherwise = value1 - value2
                                        in ra (value,input2)  
     Nothing -> Nothing 
ra (value1, input) = Just (value1, input)

rf :: String -> Maybe (Int, String) 
rf ('(':input) = case le input of 
     Just (value,(')':input2)) -> Just (value, input2) 
     _ -> Nothing  

rf (d:input) | isDigit d = Just (digitToInt d, input) 
rf _ = Nothing

-- Task 6 -----------------------------------------
evalPrior :: String -> Maybe Int 
evalPrior = undefined

pe :: String -> Maybe String 
pe = undefined

pa :: (Int,String) -> Maybe (Int,String) 
pa = undefined

pt :: String -> Maybe String 
pt = undefined

pb :: (Int,String) -> Maybe (Int,String) 
pb = undefined

pf :: String -> Maybe String 
pf = undefined

------------------------------------------------------
match :: Char -> Maybe String -> Maybe String 
match c1 (Just (t:st)) | c1==t = Just st
match _ _                      = Nothing 

inOp :: Char -> Int -> Int -> Int
inOp c2 = case c2 of {'+' -> (+); '-' -> (-); '*' -> (*); _ -> undefined}