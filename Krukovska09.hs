{-# OPTIONS_GHC -Wall #-}
module Krukovska09 where

import Data.List
import qualified Text.ParserCombinators.Parsec as P

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- Task 1 -----------------------------------------
simplify :: RE -> RE   
simplify (Null) = Null
simplify (Term char) = Term char
simplify (Seq reA reB) = (Seq (simplify reA) (simplify reB))
simplify (Alt reA reB) = (Alt (simplify reA) (simplify reB))
simplify (Rep reA) = Rep (simplify reA)
simplify (Plus re) = Seq (simplify re) (Rep (simplify re))
simplify (Opt re) = Alt (simplify re) Null

-- Task 2 -----------------------------------------
isTerminal :: Automation -> State -> Bool 
isTerminal (_, endStates, _) s = elem s endStates

isEssential :: Automation -> State -> Bool 
isEssential aut s = isTerminal aut s || readSymbol aut s

readSymbol :: Automation -> State -> Bool
readSymbol (_, _, []) _ = False
readSymbol (state, endStates, transitions) s | checkState (head transitions) s = True
                                             | otherwise = readSymbol (state, endStates, (tail transitions)) s
                                           where checkState :: Transition -> State -> Bool
                                                 checkState (st, _, C _) ss = st == ss
                                                 checkState (_, _, Eps) _ = False

-- Task 3 -----------------------------------------
transitionsFrom :: Automation -> State -> [Transition]
transitionsFrom (_, _, transitions) s = filter (\(st, _, _) -> s == st) transitions

-- Task 4 -----------------------------------------
labels :: [Transition] -> [Label]
labels trx = nub ([labelX | (_, _, labelX) <- trx, labelX /= Eps])

-- Task 5 -----------------------------------------
acceptsDA :: Automation -> String -> Bool
acceptsDA = undefined

-- Task 6 -----------------------------------------
stStep  :: Automation -> State -> Label -> [State]
setStep :: Automation -> [State] -> Label -> [State]
closure :: Automation -> [State] -> [State]

stStep  = undefined
setStep = undefined
closure = undefined

-- Task 7 -----------------------------------------
accepts :: Automation -> String -> Bool
accepts = undefined

-- Task 8 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int) 
make = undefined

-- Task 9 -----------------------------------------
parseReg :: String -> Maybe RE 
parseReg = undefined

-- Task 10 -----------------------------------------
makeDA' :: Automation -> (MetaState, [MetaState], [MetaTransition])
makeDA' = undefined  

makeDA :: Automation -> Automation
makeDA  = undefined
-------------------------------------------------------
-- showRE - function that can be useful while testing
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re) = showRE' re ++ "*"
showRE (Plus re) = showRE' re ++ "+"
showRE (Opt re) = showRE' re ++ "?"
showRE re = showRE' re

showRE' :: RE -> String
showRE' Null = ""
showRE' (Term c) = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Test data
reFigureS, re1S, re2S, re3S, re4S, re5S, re6S :: String
reFigureS = "(a|b)*c"
re1S = "(x|y)(1|2)"
re2S = "x'*"
re3S = "(ab|c)*"
re4S = "(a?)a"
re5S = "(ab)?d+"
re6S = "c?*"

reFigure, re1, re2, re3, re4, re5, re6 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Opt(Term 'a')) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))
re6 = Rep (Opt (Term 'c'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, nda6, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5, da6 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

nda6 = (1,[2], [(1,2,Eps),(1,3,Eps),(3,5,Eps),(5,6, C 'c'), (6,4,Eps), 
                (4,2,Eps), (3,7,Eps), (7,8,Eps), (8,4,Eps), (4,3,Eps)]) 
da6 = (1,[1], [(1,1, C 'c')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )
