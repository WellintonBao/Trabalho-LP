{
module Parser where 

import Lexer 

}

%name parser 
%tokentype { Token }
%error { parseError }

%left '||'
%left "&&"
%left "==" "!=" 
%left '+' '-'
%left '*'

%token 
    num             { TokenNum $$ }
    true            { TokenTrue }
    false           { TokenFalse }
    '+'             { TokenPlus }
    '*'             { TokenTimes }
    "&&"            { TokenAnd }
    "||"            { TokenOr }
    '('             { TokenLParen }
    ')'             { TokenRParen }
    "=="            { TokenEq }
    "!="            { TokenNeq }
    '{'             { TokenLBrace }
    '}'             { TokenRBrace }
    'if'            { TokenIf }
    'then'          { TokenThen }
    'else'          { TokenElse }
    '.'             { TokenDot }
    ':'             { TokenAttri }
    ','             { TokenComma}
    'record'        { TokenRecord }
    id              { TokenId $$ }


%% 

Exp     : num                             { Num $1 }
        | true                            { BTrue }
        | false                           { BFalse }
        | Exp '+' Exp                     { Add $1 $3 }
        | Exp '*' Exp                     { Times $1 $3 }
        | Exp "&&" Exp                    { And $1 $3 }
        | Exp "||" Exp                    { Or $1 $3 }
        | '(' Exp ')'                     { Paren $2 }
        | Exp "==" Exp                    { Eq $1 $3 }
        | Exp "!=" Exp                    { Neq $1 $3 }
        | id                              { Var $1 }
        | 'if' Exp 'then' Exp 'else' Exp  { If $2 $4 $6 } 
        | Exp '.' id                      { AccessRecord $1 (Var $3) }
        | Record                          { $1 }

RecordField : id ':' Exp                   { RecordField (Var $1) $3 }

Record      : 'record' '{' '}'          { Record [] }           
            | 'record' '{' RecordFieldList '}' { Record $3 } 

RecordFieldList
        : RecordField                         { [$1] }
        | RecordField ',' RecordFieldList     { $1 : $3 }

                
{ 

parseError :: [Token] -> a 
parseError _ = error "Syntax error!"

}
