{-# OPTIONS_GHC -Wall #-}
module Krukovska09 where

import Data.List
import Data.Maybe(fromMaybe)
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
getState :: Automation -> State
getState (st, _, _) = st

getSndState :: Transition -> State
getSndState (_, endStates, _) = endStates

acceptsDAHelper :: Automation -> State -> String -> Bool
acceptsDAHelper daut state "" = isTerminal daut state
acceptsDAHelper daut state (s:st) | isTerminal daut state = False 
                                  | otherwise = if null res then False else acceptsDAHelper daut (getSndState(head res)) st
                                where res = filter (\(_, _, label) -> (C s) == label) (transitionsFrom daut state)

acceptsDA :: Automation -> String -> Bool
acceptsDA daut st = acceptsDAHelper daut (getState daut) st

-- Task 6 -----------------------------------------
getNextState :: Transition -> State
getNextState (_, nextState, _) = nextState

stStep :: Automation -> State -> Label -> [State]
stStep naut st mc = map getNextState (filter (\(_, _, label) -> label == mc) (transitionsFrom naut st))

setStep :: Automation -> [State] -> Label -> [State]
setStep naut bs mc = concatMap (\st -> stStep naut st mc) bs

notStateElem :: State -> [State] -> Bool
notStateElem st stateList = null [x |x <- stateList, x == st]

closure :: Automation -> [State] -> [State]
closure naut st | null (filter (\x -> notStateElem x st) stepN) = st
                | otherwise = sort (nub (closure naut (stepN ++ st)))
                where stepN = setStep naut st Eps

-- Task 7 -----------------------------------------
acceptsHelper :: Automation -> String -> [State] -> Bool
acceptsHelper _ _ [] = False
acceptsHelper aut [] states = elem True (map (isTerminal aut) states)
acceptsHelper aut (s:st) states = acceptsHelper aut st (setStep aut (closure aut states) (C s))     

accepts :: Automation -> String -> Bool
accepts aut st = acceptsHelper aut st [(getState aut)]                    

-- Task 8 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int) 
make Null beg fin nxt = ([(beg, fin, Eps)], nxt)
make (Plus _) _ _ _ = undefined
make (Opt _) _ _ _ = undefined
make (Term t) beg fin nxt = ([(beg, fin, C t)], nxt)
make (Seq r1 r2) beg fin nxt = let s1 = make r1 beg nxt (nxt + 2)
                                   s2 = make r2 (nxt + 1) fin (snd s1)
                               in ((fst s1) ++ [(nxt, nxt + 1, Eps)] ++ (fst s2), (snd s2))
make (Alt r1 r2) beg fin nxt = let s1 = make r1 nxt (nxt + 1) (nxt + 4)
                                   s2 = make r2 (nxt + 2) (nxt + 3) (snd s1)
                               in ([(beg, nxt, Eps)] ++ (fst s1) ++ [(nxt + 1, fin, Eps), (beg, nxt + 2, Eps)] ++ (fst s2) ++ [(nxt + 3, fin, Eps)], snd s2)
make (Rep r1) beg fin nxt = let r2 = make r1 nxt (nxt + 1) (nxt + 2)
                            in ([(beg, fin, Eps), (beg, nxt, Eps)] ++ (fst r2) ++ [(nxt + 1, nxt, Eps), (nxt + 1, fin, Eps)], (snd r2))

-- Task 9 -----------------------------------------
reg :: P.Parser RE
reg = do r <- rexpr
         P.eof
         return r

rexprHelper :: P.Parser RE
rexprHelper = do _ <- P.char '|'
                 r <- rfact
                 return r

altHelper :: [RE] -> RE
altHelper [] = Null
altHelper (x:[]) = x
altHelper (x:xs) = Alt x (altHelper xs)

rexpr :: P.Parser RE
rexpr = do r1 <- rterm
           r2 <- P.many (rexprHelper)
           return (if null r2 then r1 else altHelper (r1:r2))

sequenceHelper :: [RE] -> RE
sequenceHelper [] = Null
sequenceHelper (x:[]) = x
sequenceHelper (x:xs) = Seq x (sequenceHelper xs)

rterm :: P.Parser RE
rterm = do r1 <- rfact
           r2 <- P.many rfact
           return (if null r2 then r1 else sequenceHelper(r1:r2))

convert :: RE -> String -> RE
convert r [] = r
convert r (x:xs) | x == '*' = Rep (convert r xs)
                 | x == '?' = Opt (convert r xs)
                 | x == '+' = Plus (convert r xs)
convert _ _ = Null

rfact :: P.Parser RE
rfact = do r <- prime
           o <- P.many (P.oneOf "*?+")
           return $ convert r (reverse o)

prime :: P.Parser RE
prime = (P.try rsymb) P.<|> primeHelper

primeHelper :: P.Parser RE
primeHelper = do _ <- P.char '('
                 r <- rexpr
                 _ <- P.char ')'
                 return r

rsymb :: (P.Parser) RE
rsymb = do symbols <- P.noneOf "()|*+?"
           return (Term symbols)

parseReg :: String -> Maybe RE 
parseReg str = case P.parse reg "" str of
                Left _ -> Nothing
                Right res -> Just res

-- Task 10 -----------------------------------------
getEndStates :: Automation -> [State]
getEndStates (_, endStates, _) = endStates

buildMetastate :: Automation -> MetaState -> [Label] -> [MetaState]
buildMetastate aut msx l = map ((closure aut).(setStep aut msx)) l

makeStep :: Automation -> ([MetaState], [MetaState], [MetaTransition]) -> ([MetaState], [MetaState], [MetaTransition])
makeStep _ x@(_, [], _) = x
makeStep aut (gmsx,(msx:bmsx), mtrx) = let l = nub (concatMap (labels.(transitionsFrom aut)) msx)
                                       in addStates (gmsx ++ [msx], bmsx, mtrx) msx (map (\x -> filter (isEssential aut) x) (buildMetastate aut msx l)) l

addStates :: ([MetaState], [MetaState], [MetaTransition]) -> MetaState -> [MetaState] -> [Label] -> ([MetaState], [MetaState], [MetaTransition])
addStates x@(gmsx, bmsx, mtrx) msx mlx l = let mlxHead = head mlx
                                               newBmsx = if elem mlxHead bmsx || elem mlxHead gmsx then bmsx else bmsx ++ [mlxHead]
                                               newMtrx = mtrx ++ [(msx, mlxHead, head l)]
                                           in if null mlx then x else addStates (gmsx, newBmsx ,newMtrx) msx (tail mlx) (tail l) 

newNormSt :: ([MetaState], [MetaState], [MetaTransition]) -> MetaState -> [MetaState] -> [Label] -> ([MetaState], [MetaState], [MetaTransition])
newNormSt (gmsx, bmsx, mtrx) msx mlx l |null mlx = (gmsx, bmsx, mtrx)
                                       |otherwise = addStates (gmsx, newBmsx, newMtrx) msx (tail mlx) (tail l) 
                                        where mlxHead = head mlx
                                              newBmsx = if elem mlxHead bmsx || elem mlxHead gmsx then bmsx else bmsx ++ [mlxHead]
                                              newMtrx = mtrx ++ [(msx, mlxHead, head l)]

endCycleCond :: ([MetaState], [MetaState], [MetaTransition]) -> Bool
endCycleCond (_, bmsx, _) = null bmsx

comparator :: (Int, Int, Label) -> (Int, Int, Label) -> Ordering
comparator (a1, b1, _) (a2, b2, _) | a1 > a2 = GT
                                   | a1 < a2 = LT
                                   | otherwise = compare b1 b2

makeDA' :: Automation -> (MetaState, [MetaState], [MetaTransition])
makeDA' aut = let (st, _, l) = until endCycleCond (makeStep aut)([],[filter (\x -> (isEssential aut x)) (closure aut [(getState aut)])],[]) in (head st, st, l)

makeDA :: Automation -> Automation
makeDA aut = let (_, mstx, mtrx) = makeDA' aut
                 endStates = [x + 1 | x <- [0..(length mstx) - 1], not (null (intersect (mstx !! x) (getEndStates aut)))]
                 transitions = map (\(a, b, c) -> ((fromMaybe (-1) $ elemIndex a mstx) + 1, (fromMaybe (-1) $ elemIndex b mstx) + 1, c)) mtrx
             in (1, endStates, (sortBy comparator transitions))

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
