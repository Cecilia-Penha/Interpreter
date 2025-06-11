module Interpreter where 

import Lexer 

isValue :: Expr -> Bool 
isValue BTrue       = True 
isValue BFalse      = True  
isValue (Num _)     = True 
isValue (Lam _ _ _) = True
isValue Nil         = True -- lista
isValue (Cons v1 v2) = isValue v1 && isValue v2
isValue _           = False 

subst :: String -> Expr -> Expr -> Expr
subst v e BTrue = BTrue 
subst v e BFalse = BFalse 
subst v e (Num x) = Num x 
subst v e (Add e1 e2) = Add (subst v e e1) (subst v e e2)
subst v e (Sub e1 e2) = Sub (subst v e e1) (subst v e e2) -- adicionado sub
subst v e (Or e1 e2) = Or (subst v e e1) (subst v e e2) -- adicionado or
subst v e (And e1 e2) = And (subst v e e1) (subst v e e2)
subst v e (If e1 e2 e3) = If (subst v e e1) (subst v e e2) (subst v e e3)
subst v e (Var x) = if v == x then 
                      e 
                    else 
                      Var x 
subst v e (Lam x t b) = Lam x t (subst v e b)
subst v e (App e1 e2) = App (subst v e e1) (subst v e e2)
subst v e (Paren e1) = Paren (subst v e e1)
subst v e (Eq e1 e2) = Eq (subst v e e1) (subst v e e2)
subst v e Nil = Nil --lista
subst v e (Cons h t) = Cons (subst v e h) (subst v e t)
subst v e (IsNil x) = IsNil (subst v e x)
subst v e (Head x) = Head (subst v e x)
subst v e (Tail x) = Tail (subst v e x)

step :: Expr -> Expr 
step (Add (Num n1) (Num n2)) = Num (n1 + n2)     -- S-Add
step (Add (Num n1) e2) = let e2' = step e2       -- S-Add2
                           in Add (Num n1) e2' 
step (Add e1 e2) = Add (step e1) e2              -- S-Add1
step (Sub (Num n1) (Num n2)) = Num (n1 - n2)     -- S-Sub
step (Sub (Num n1) e2) = let e2' = step e2       -- S-Sub2
                           in Sub (Num n1) e2'
step (Sub e1 e2) = Sub (step e1) e2              -- S-Sub1
step (And BTrue e2) = e2 
step (And BFalse e2) = BFalse 
step (And e1 e2) = And (step e1) e2 
step (Or BTrue e2) = BTrue -- adicionado or
step (Or BFalse e2) = e2 -- adicionado or
step (Or e1 e2) = Or (step e1) e2 -- adicionado or
step (If BTrue e1 e2) = e1 
step (If BFalse e1 e2) = e2
step (If e e1 e2) = If (step e) e1 e2 
step (App e1@(Lam x t b) e2) | isValue e2 = subst x e2 b
                             | otherwise  = App e1 (step e2)
step (App e1 e2) = App (step e1) e2 
step (Paren e) = e 
step (Eq (Num n1) (Num n2)) = if n1 == n2 then BTrue else BFalse -- adicionado eq
step (Eq BTrue BTrue) = BTrue
step (Eq BFalse BFalse) = BTrue
step (Eq BTrue BFalse) = BFalse
step (Eq BFalse BTrue) = BFalse
step (Eq e1 e2)
  | not (isValue e1) = Eq (step e1) e2
  | not (isValue e2) = Eq e1 (step e2)
step (IsNil Nil) = BTrue --lista
step (IsNil (Cons _ _)) = BFalse
step (IsNil e) = IsNil (step e)
step (Head (Cons h _)) = h
step (Head e) = Head (step e)
step (Head Nil) = Nil
step (Tail (Cons _ t)) = t
step (Tail e) = Tail (step e)
step (Tail Nil) = Nil
step (Cons e1 e2)
  | not (isValue e1) = Cons (step e1) e2
  | not (isValue e2) = Cons e1 (step e2)
step e = error ("step: erro " ++ show e)

eval :: Expr -> Expr 
eval e | isValue e = e 
       | otherwise = eval (step e)

