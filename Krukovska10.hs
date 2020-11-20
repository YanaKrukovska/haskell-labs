{-# OPTIONS_GHC -Wall #-}
module Krukovska10 where

import Data.List

-- only work with scalars and arrays
--------------------------------------------------------------------
type Id    = String
data Value = I Int | A [(Int, Int)]  deriving (Eq, Show)
data Op    = Add | Minus | Mul | Less | Equal | Index  deriving (Eq, Show)

data Exp = Const Int 
         | Var Id 
         | OpApp Op Exp Exp 
         | Cond Exp Exp Exp 
         | FunApp Id [Exp] 
         deriving (Eq, Show)

data Stmt = Assign Id Exp 
          | AssignA Id Exp Exp 
          | If Exp Stmt Stmt 
          | While Exp Stmt 
          | Call Id [Exp] 
          | Block [VarDef] [Stmt]
          deriving (Eq, Show)

data VarDef  =  Arr Id | Int Id deriving (Eq, Show)

type FunDef  =  (Id, ([VarDef], Exp))
-- functions return only scalars, don't use global data
type ProcDef = (Id, ([VarDef], Stmt))
type Program = ([VarDef], [FunDef], [ProcDef])

type StateP  = [(Id, Value)]  -- data stack

data Type    = At | It  deriving (Eq, Show)
type FunEnv  = [(Id,[Type])]
type ProcEnv = [(Id,[Type])]
type VarEnv  = [(Id,Type)]

-- Task 1 ------------------------------------
updateValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
updateValue a b [] = [(a, b)]
updateValue a b (x:arr) | fst x == a = [(fst x, b)] ++ arr
                        | otherwise = [x] ++ (updateValue a b arr)

-- Task 2 ------------------------------------
updateArray :: Value -> Value -> Value -> Value
updateArray (A a) (I i) (I v) = A (updateValue i v a)
updateArray a _ _ = a

-- Task 3 ------------------------------------
arrayContains :: Eq a => a -> [(a,b)] -> Bool
arrayContains a ((x, _):arr) | a == x = True
                             | otherwise = arrayContains a arr
arrayContains _ [] = False

applyOp :: Op -> Value -> Value -> Value 
applyOp Add (I v1) (I v2) = I (v1 + v2)
applyOp Minus (I v1) (I v2) = I (v1 - v2)
applyOp Mul (I v1) (I v2) = I (v1 * v2)
applyOp Less (I v1) (I v2) | v1 < v2 = (I 1)
                           | otherwise = (I 0)
applyOp Equal (I v1) (I v2)| v1 == v2 = (I 1) 
                           | otherwise = (I 0)
applyOp Index (A arr) (I v) | arrayContains v arr = I (lookUp v arr)
                            | otherwise = (I 0)
applyOp _ _ _ = error "wrong input"

-- Task 4 ------------------------------------
getId :: VarDef -> Id
getId (Arr idV) = idV
getId (Int idV) = idV

evExp ::  Exp -> [FunDef] -> StateP -> Value 
evExp (Const c) _ _ = I c
evExp (Var v) _ st = lookUp v st
evExp (OpApp o x1 x2) dfx st = applyOp o (evExp x1 dfx st) (evExp x2 dfx st)
evExp (Cond x1 x2 x3) dfx st | (evExp x1 dfx st) == I 0 = evExp x3 dfx st
                             | otherwise = evExp x2 dfx st
evExp (FunApp idV es) dfx st = let (asVars, ef) = lookUp idV dfx
                                   vs = evArgs es dfx st
                                   new = zip (map getId asVars) vs
                               in evExp ef dfx new

evArgs :: [Exp] -> [FunDef] -> StateP -> [Value]  
evArgs ex dfx st = map (\x -> evExp x dfx st) ex

-- Task 5 ------------------------------------
updateState :: Id -> [Exp] -> [FunDef] -> [ProcDef] -> StateP -> StateP
updateState i ex dfx dpx st = let (vs, _) = lookUp i dpx in (zip (map getId vs) (map (\e -> evExp e dfx st) ex)) ++ st

addLocalVariables :: [VarDef] -> StateP -> StateP
addLocalVariables [] st = st
addLocalVariables (v:vs) st = addLocalVariables vs ((initv v):st)

evBlock :: [Stmt] -> [FunDef]->[ProcDef]->StateP->StateP
evBlock [] _ _ st = st
evBlock (s:ts) dfx dpx st = evBlock ts dfx dpx (evStmt s dfx dpx st) 

deleteLocal :: [VarDef] -> StateP -> StateP
deleteLocal [] st = st
deleteLocal [Arr v] st = deleteVars v st
deleteLocal [Int v] st = deleteVars v st
deleteLocal ((Arr v):vs) st = deleteLocal vs (deleteVars v st)
deleteLocal ((Int v):vs) st = deleteLocal vs (deleteVars v st)

deleteVars ::Id -> StateP -> StateP
deleteVars _ [] = []
deleteVars i ((d, s):st) | i == d = st
                         | otherwise = (d,s):(deleteVars i st)

evStmt :: Stmt -> [FunDef] -> [ProcDef] -> StateP -> StateP
evStmt (Assign i ex) dfx _ st = updateValue i (evExp ex dfx st) st
evStmt (AssignA i ex1 ex2) dfx _ st = updateValue i (updateArray (lookUp i st) (evExp ex1 dfx st)(evExp ex2 dfx st)) st
evStmt (If ex stmt1 stmt2) dfx dpx st = let I i = evExp ex dfx st in evStmt (if i /= 0 then stmt1 else stmt2) dfx dpx st
evStmt (While ex stmt) dfx dpx st = let cond stN = let I i = evExp ex dfx stN in i == 0 
                                        step stN = evStmt stmt dfx dpx stN 
                                    in until cond step st
evStmt (Call i ex) dfx dpx st = let (vs, stmt) = lookUp i dpx 
                                    res = evStmt stmt dfx dpx (updateState i ex dfx dpx st)
                                 in deleteLocal vs res
evStmt (Block vd stmt) dfx dpx st = deleteLocal vd (evBlock stmt dfx dpx (addLocalVariables vd st))

-- Task 6 ------------------------------------
iswfExp :: Exp -> VarEnv -> FunEnv -> Maybe Type
iswfExp (Const _) _ _ = Just It
iswfExp (Var v) ve _ | isElem v ve = Just ([b | (a, b) <- ve, a == v] !! 0)
                     | otherwise = Nothing
                     where isElem :: Id -> VarEnv -> Bool
                           isElem e (vv:vs) | fst vv == e = True
                                            | otherwise = isElem e vs
                           isElem _ [] = False
iswfExp (OpApp op x1 x2) ve fe = case (iswfExp x1 ve fe) of
         Just a -> case (iswfExp x2 ve fe) of
              Just b -> iswfOp op [a, b]
              _ -> Nothing
         _ -> Nothing
iswfExp (Cond x1 x2 x3) ve fe = case (iswfExp x1 ve fe) of
    Just a -> case (iswfExp x2 ve fe) of
        Just b -> case (iswfExp x3 ve fe) of
            Just c -> iswfCond [a, b, c]
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
iswfExp (FunApp i ex) ve fe | f == exs = Just It 
                            | otherwise = Nothing
                             where f = map Just (lookUp i fe)
                                   exs = map (\x -> iswfExp x ve fe) ex

-- Task 7 ------------------------------------
getType :: VarDef -> (Id, Type)
getType (Arr a) = (a, At)
getType (Int i) = (i, It)

iswfStmt :: Stmt -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfStmt (Assign i ex) ve fe _ = lookup i ve == iswfExp ex ve fe && lookup i ve == Just It
iswfStmt (AssignA i ex1 ex2) ve fe _ = let Just t1 = lookup i ve
                                           Just t2 = iswfExp ex1 ve fe
                                           Just t3 = iswfExp ex2 ve fe
                                        in iswfAssignA [t1, t2, t3]
iswfStmt (If ex stmt1 stmt2) ve fe pe = Just It == iswfExp ex ve fe && iswfStmt stmt1 ve fe pe && iswfStmt stmt2 ve fe pe
iswfStmt (While ex stmt) ve fe pe = Just It == iswfExp ex ve fe && iswfStmt stmt ve fe pe
iswfStmt (Call i ex) ve fe pe = map (\x -> iswfExp x ve fe) ex == (map Just (lookUp i pe))
iswfStmt (Block vd stmt) ve fe pe = all (\s -> iswfStmt s (map getType vd ++ ve) fe pe) stmt

-- Task 8 ------------------------------------
getTypeOnly :: VarDef -> Type
getTypeOnly (Arr _) = At
getTypeOnly (Int _) = It

iswfFunDef :: FunDef -> FunEnv -> Bool
iswfFunDef (i, (vd, ex)) fe = elem (i, map getTypeOnly vd) fe && iswfExp ex (map getType vd ) fe /= Nothing
                      
iswfProcDef :: ProcDef -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfProcDef (i, (procTypes, stmt)) ve fe pe = let updatedTypes = map getTypeOnly procTypes
                                                  updatedVe = map getType procTypes ++ ve
                                                  updatedPe = updateValue i updatedTypes pe 
                                              in elem (i, updatedTypes) updatedPe && iswfStmt stmt updatedVe fe updatedPe

-- Task 9 ------------------------------------
checkFuns :: [FunDef] -> [(Id,[Type])] -> Bool
checkFuns fd fs = and (map (\f -> iswfFunDef f fs) fd)

containsMain :: [(Id,[Type])] -> Bool
containsMain pr = elem ("main", []) pr

checkIds :: [Id] -> Bool
checkIds ids = length ids == length (nub ids)

createFunEnv :: FunDef -> (Id,[Type])
createFunEnv  (i, (fd, _)) = (i, map getTypeOnly fd)

createProcEnv :: ProcDef -> (Id,[Type])
createProcEnv  (i, (vd, _)) = (i, map getTypeOnly vd)

iswfProgram :: Program -> Bool
iswfProgram (vd, fd, pd) = checkFuns fd fs && checkProc && containsMain pr && checkIds ids
                           where vs = map getType vd 
                                 fs = map createFunEnv fd
                                 pr = map createProcEnv pd
                                 checkProc = and (map (\p -> iswfProcDef p vs fs pr) pd)
                                 ids = (map fst vs) ++ (map fst fs) ++ (map fst pr)

--- Helper functions -----------------------------
lookUp :: Eq a => a -> [(a,b)] -> b
-- Precondition: par with key a is in list of pairs abx
lookUp a abx = maybe (error "lookUp") id (lookup a abx) 

-- forms initial value of variable
initv :: VarDef -> (Id, Value)
initv (Arr v) = (v, A [])
initv (Int v) = (v, I 0) 

-- Implementation of program 
evProgram :: Program -> StateP 
evProgram (dvx, dfx, dpx) = 
   let sb = map initv dvx 
       ( _, s) = lookUp "main" dpx      
   in  evStmt s dfx dpx sb   

--  iswfOp o ts - checks if types of operands of ts 
--     of binary operation o and forms result Just t or Nothing  
iswfOp :: Op -> [Type] -> Maybe Type 
iswfOp Add   [It,It] = Just It
iswfOp Minus [It,It] = Just It
iswfOp Mul   [It,It] = Just It
iswfOp Less  [It,It] = Just It
iswfOp Equal [It,It] = Just It
iswfOp Index [At,It] = Just It
iswfOp _      _      = Nothing

--  iswfCond ts - checks if types of operands of ts
--     of "if" and forms result Just t або Nothing 
iswfCond :: [Type] -> Maybe Type 
iswfCond [It,It,It] = Just It
iswfCond [It,At,At] = Just At
iswfCond _          = Nothing 

-- iswfAssignA ts - checks if types of operands of ts
--   for assigning operation
iswfAssignA :: [Type] -> Bool
iswfAssignA [At,It,It] = True 
iswfAssignA _          = False  

---- Test data  -----------------------
-- State for testing
sampleState :: StateP
sampleState = [("x",I 5),("y",I 2),("a", A [(2,3),(0,4), (1,2)])]

varEnv :: VarEnv 
varEnv = [("x",It), ("y",It), ("a",At)]

-- Function - max of two numbers
-- func biggest(m,n)= (m<n ? n : m)
biggest :: FunDef
biggest =("biggest",
          ([Int "m", Int "n"], 
           Cond (OpApp  Less (Var "m") (Var "n"))  (Var "n")  (Var "m")                                                                
           )
         )
-- Function to count Fibonacci number
-- func fib(n) = (n<3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
       ([Int "n"], 
        Cond (OpApp Less (Var "n") (Const 3))
             (Const 1)
             (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const 1)])
                        (FunApp "fib" [OpApp Minus (Var "n") (Const 2)]))
       )
      )

-- Function - sum of all elements of array 0..n ...
-- func sumA(a[],n) = (n<0 ? 0 : a[n] + sumA (a,n-1))
sumA :: FunDef
sumA = ("sumA",
        ([Arr "a", Int "n"],
         Cond (OpApp Less (Var "n") (Const 0)) 
              (Const 0)
              (OpApp Add (OpApp Index (Var "a") (Var "n"))
                         (FunApp "sumA" [Var "a", OpApp Minus (Var "n")(Const 1)])
              )
        )
       )

funEnv :: FunEnv
funEnv = [("biggest",[It,It]),("fib", [It]),("sumA",[At,It])]

-- Example of operator - block
sampleBlock :: Stmt 
sampleBlock = Block [Arr "b"]
                 [AssignA "b" (Const 0) (Const 9), AssignA "b" (Const 2) (Const 5),
                  AssignA "b" (Const 3) (Const 7), AssignA "b" (Const 5) (Const 1),
                  Call "sumA1" [Var "b", Const 5]
                 ]

-- Procedure - addition of two numbers...
-- proc gAdd(x,y) gSum = x + y 
gAdd :: ProcDef
gAdd = ("gAdd", 
        ([Int "x", Int "y"], 
         Assign "gSum" (OpApp Add (Var "x") (Var "y"))
        )
       )

-- Procedure - sum of all elements of array 0..n ...
-- proc sumA1(a[],n) {i;limit;
--      sA=0; i=0; limit=n+1;
--      while (i<limit){sA=sA+a[i]; i=i+1}
--                   }
sumA1 :: ProcDef
sumA1 = ("sumA1",
         ([Arr "a", Int "n"], 
          Block [Int "i", Int "limit"] 
            [Assign "sA" (Const 0), Assign "i" (Const 0),
             Assign "limit" (OpApp Add (Var "n") (Const 1)),
             While (OpApp Less (Var "i") (Var "limit"))
                   (Block [] 
                     [Assign "sA" (OpApp Add (Var "sA")
                                  (OpApp Index (Var "a") (Var "i"))),
                      Assign "i" (OpApp Add (Var "i") (Const 1))
                     ]
                   )
            ]
         )
        )

procEnv :: ProcEnv 
procEnv = [("gAdd",[It,It]),("sumA1",[At,It])]

-- Full programmes
-- gSum; 
-- proc gAdd(x,y) gSum = x + y 
-- proc main() call gAdd(5,10)   
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],Call "gAdd" [Const  5, Const 10]))])

-- sA
-- proc sumA1(a[],n) {i;limit; .... } 
-- proc main() {b[]; b[0]=9; b[2]=5; b[3]=7; b[5]=1;
--                   call sumA1 (b,5)
--             }

pr2 :: Program
pr2 = ([Int "sA"], [], 
       [sumA1, 
        ("main",([], sampleBlock))
       ])
