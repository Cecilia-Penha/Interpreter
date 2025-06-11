module Lexer where 

import Data.Char

data Expr = BTrue 
          | BFalse 
          | Num Int 
          | Add Expr Expr 
          | Sub Expr Expr
          | And Expr Expr 
          | Or Expr Expr
          | If Expr Expr Expr 
          | Var String 
          | Lam String Ty Expr 
          | App Expr Expr 
          | Paren Expr 
          | Eq Expr Expr
          | Nil -- lista vazia
          | Cons Expr Expr -- construtor de lista x xs
          | IsNil Expr -- testa se e vazia
          | Head Expr
          | Tail Expr
          deriving Show 

data Ty = TBool 
        | TNum 
        | TFun Ty Ty 
        | TList Ty -- adiciona tipo lista com elementos ty
        deriving (Show, Eq)

data Token = TokenTrue 
           | TokenFalse 
           | TokenNum Int 
           | TokenAdd 
           | TokenSub
           | TokenAnd 
           | TokenOr
           | TokenIf 
           | TokenThen
           | TokenElse 
           | TokenVar String 
           | TokenLam 
           | TokenColon
           | TokenArrow 
           | TokenTNum 
           | TokenTBool
           | TokenLParen 
           | TokenRParen  
           | TokenEq
           | TokenNil
           | TokenCons
           | TokenIsNil
           | TokenHead
           | TokenTail
           | TokenList
           deriving Show 

lexer :: String -> [Token]
lexer [] = [] 
lexer ('+':cs) = TokenAdd : lexer cs 
lexer ('-':cs) = TokenSub : lexer cs -- adicionado sub
lexer ('\\':cs) = TokenLam : lexer cs 
lexer (':':cs) = TokenColon : lexer cs 
lexer ('(':cs) = TokenLParen : lexer cs 
lexer (')':cs) = TokenRParen : lexer cs 
lexer ('&':'&':cs) = TokenAnd : lexer cs 
lexer ('|':'|':cs) = TokenOr : lexer cs -- adicionado or
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('=':'=':cs) = TokenEq : lexer cs -- adicionado eq
lexer (c:cs) | isSpace c = lexer cs 
             | isDigit c = lexNum (c:cs) 
             | isAlpha c = lexKW (c:cs)



lexNum :: String -> [Token]
lexNum cs = case span isDigit cs of 
              (num, rest) -> TokenNum (read num) : lexer rest 

lexKW :: String -> [Token]
lexKW cs = case span isAlpha cs of 
             ("true", rest) -> TokenTrue : lexer rest 
             ("false", rest) -> TokenFalse : lexer rest 
             ("if", rest) -> TokenIf : lexer rest 
             ("then", rest) -> TokenThen : lexer rest 
             ("else", rest) -> TokenElse : lexer rest 
             ("Number", rest) -> TokenTNum : lexer rest 
             ("Boolean", rest) -> TokenTBool : lexer rest 
             ("nil", rest) -> TokenNil : lexer rest
             ("isnil", rest) -> TokenIsNil : lexer rest
             ("cons", rest) -> TokenCons : lexer rest
             ("head", rest) -> TokenHead : lexer rest
             ("tail", rest) -> TokenTail : lexer rest
             ("List", rest) -> TokenList : lexer rest
             (var, rest) -> TokenVar var : lexer rest 
 
