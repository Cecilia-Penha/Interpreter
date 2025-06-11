{
module Parser where 

import Lexer
}

%name parser 
%tokentype { Token }
%error { parseError }

%token 
    num             { TokenNum $$ }
    true            { TokenTrue }
    false           { TokenFalse }
    '+'             { TokenAdd }
    '-'             { TokenSub } -- adicionado sub
    "&&"            { TokenAnd }
    "||"            { TokenOr } --adiconado or
    if              { TokenIf }
    then            { TokenThen }
    else            { TokenElse }
    var             { TokenVar $$ }
    '\\'            { TokenLam }
    ':'             { TokenColon }
    "->"            { TokenArrow }
    Number          { TokenTNum }
    Boolean         { TokenTBool }
    '('             { TokenLParen }
    ')'             { TokenRParen }
    "=="            { TokenEq } -- adicionado eq
    "nil"           { TokenNil } --lista
    "isnil"         { TokenIsNil }
    "head"          { TokenHead }
    "tail"          { TokenTail }
    "cons"          { TokenCons }
    "List"          { TokenList }

%nonassoc if then else 
%nonassoc '\\' 
%left '+' '-'
%left "||" "&&"

%% 

Exp     : num                           { Num $1 }
        | true                          { BTrue }
        | false                         { BFalse }
        | Exp '+' Exp                   { Add $1 $3 }
        | Exp '-' Exp                   { Sub $1 $3 } -- adicionado sub
        | Exp "||" Exp                  { Or $1 $3 } -- adicionado or
        | Exp "&&" Exp                  { And $1 $3 }
        | Exp "==" Exp                  { Eq $1 $3 } -- adicionado eq
        | if Exp then Exp else Exp      { If $2 $4 $6 }
        | var                           { Var $1 }
        | '\\' var ':' Type "->" Exp    { Lam $2 $4 $6 }
        | Exp Exp                       { App $1 $2 }
        | '(' Exp ')'                   { Paren $2 }
        | "nil"                         { Nil } -- lista
        | "isnil" Exp                   { IsNil $2 }
        | "head" Exp                    { Head $2 }
        | "tail" Exp                    { Tail $2 }
        | "cons" Exp Exp                { Cons $2 $3 }


Type    : Boolean                       { TBool }
        | Number                        { TNum }
        | '(' Type "->" Type ')'        { TFun $2 $4 }
        | "List" Type                   { TList $2 }

{ 

parseError :: [Token] -> a 
parseError _ = error "Erro sintático!"

}