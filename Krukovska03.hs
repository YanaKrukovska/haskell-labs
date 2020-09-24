{-# OPTIONS_GHC -Wall #-}
module Krukovska03 where

type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

data Command = Z Int | S Int | T Int Int | J Int Int Int deriving (Show, Eq)
type Program = [Command]
type ConfigС = (Int, Int, [Int])

-- Task 1 ------------------------------------
isPrefix :: String -> String -> Bool 
isPrefix bs xs = bs == take (length bs) xs

-- Task 2 ------------------------------------
substitute :: Substitution -> Int -> String -> String
substitute (xs, ys, _) i w = take i w ++ ys ++ drop (i + length xs) w

-- Task 3------------------------------------
getFirstParam :: Substitution -> String
getFirstParam (a,_,_) = a

findPosition :: String -> Substitution -> [(Substitution, Int)]
findPosition w sub = [(sub,x) | x <- [0..length w], isPrefix (getFirstParam sub) (drop x w)]

-- Task 4 ------------------------------------
findAllOptions :: Algorithm -> String -> [(Substitution,Int)]  
findAllOptions [] _ = []
findAllOptions (x:xs) w = findPosition w x ++ findAll xs w

findAll :: Algorithm -> String -> [(Substitution,Int)]  
findAll algo w | null algo = []
               | otherwise =  findAllOptions algo w

-- Task 5 ------------------------------------
stepA :: Algorithm -> ConfigA -> ConfigA
stepA algo (bt, st, word) = if (not bt) then (bt, st, word) else (not pr, st + 1, substitute (xs, ys, pr) i word) where ((xs, ys, pr), i) = head (findAll algo word)

-- Task 6 ------------------------------------
evalA :: Algorithm -> Int -> String -> Maybe String 
evalA algo m word = if (m == 0) then Nothing else if bt then evalA algo (m-1) w else Just w where (bt,_,w) = stepA algo (True,0,word)

-- Task 7 ------------------------------------
compareRegisters :: Command -> Int
compareRegisters (Z x)= x
compareRegisters (S x) = x
compareRegisters (T x y) = max x y
compareRegisters (J x y _) = max x y

maximReg :: Program -> Int 
maximReg pr = if (null pr) then 1 else max (compareRegisters(head pr)) (maximReg (tail pr))

-- Task 8 ------------------------------------
ini :: Program -> [Int] -> [Int] 
ini pr ir = if (length ir < maximReg pr) then ir ++ [0|_ <- [1..(maximReg pr - length ir)]] else take (maximReg pr) ir

upd :: [Int] -> Int -> Int-> [Int]
upd reg r v = take r reg ++ [v] ++ drop (r + 1) reg

-- Task 9 ------------------------------------
applyCommand :: Command -> ConfigС -> ConfigС
applyCommand (Z x) (a, b, reg) = (a + 1, b + 1, upd reg (x - 1) 0) 
applyCommand (S x) (a, b, reg) = (a + 1, b + 1, upd reg (x - 1) ((reg !! (x - 1)) + 1))
applyCommand (T x y) (a, b, reg) = (a + 1, b + 1, upd reg (y - 1) (reg !! (x - 1)))
applyCommand (J x y z) (a, b, reg) = if ((reg !! (x - 1)) == (reg !! (y - 1))) then(z, b + 1, reg) else (a + 1, b + 1, reg)

stepC :: Program -> ConfigС -> ConfigС
stepC pr (a, b, reg) = applyCommand (pr !! (a - 1)) (a, b, reg)

-- Task 10 ------------------------------------
getFirstConfigParam :: ConfigС -> Int
getFirstConfigParam (a, _, _) = a

getThirdConfigParam :: ConfigС -> [Int]
getThirdConfigParam (_, _, c) = c

calculateEvalC :: Program -> Int -> [Int] -> Int -> Maybe Int
calculateEvalC _ 0 _ _ = Nothing
calculateEvalC pr mx ir num | (length pr < num) = Just (head ir)
               | otherwise = calculateEvalC pr (mx - 1) (getThirdConfigParam (stepC pr (num, mx, ir))) (getFirstConfigParam (stepC pr (num, mx, ir)))

evalC :: Program -> Int -> [Int] -> Maybe Int 
evalC pr mx ir = calculateEvalC pr (mx + 1) ir 1

---------------------Test data - Markov algorithm ---------------------------
clearBeginOne, addEnd, reverse, multiply:: Algorithm 
-- removes first symbol of a word {a,b}
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ] 

-- adds abb to the end of a word {a,b}
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ] 
-- {a,b}
reverse = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False) 
          , ("db", "bd", False) 
          , ("d", "", True) 
          , ("caa", "aca", False) 
          , ("cab", "bca", False) 
          , ("cba", "acb", False)
          , ("cbb", "bcb", False) 
          , ("", "c", False) 
          ]
 
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]

---------------------Test data - Register machine ---------------------------
notSignum, addition, subtraction :: Program 
-- notSignum x
notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1] 

-- addition x y = x+y
addition = [Z 3, J 3 2 6, S 1, S 3, J 1 1 2]

-- subtraction x y = x-y, where x>=y 
subtraction = [Z 3, J 1 2 6, S 2, S 3, J 1 1 2, T 3 1]
