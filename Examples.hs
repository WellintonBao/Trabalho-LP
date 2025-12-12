module Examples (
    exec, 
    ex1, 
    ex2, 
    ex3, 
    ex4, 
    ex5
) where

import Lexer
import Parser (parser) 
import Interpreter
import TypeChecker

exec :: String -> Expr
exec input = 
    let tokens = lexer input
        ast = parser tokens 
        typedAst = typecheck ast 
    in eval typedAst

--criar record vazio
ex1 :: String
ex1 = "record {}"

--exemplo para criar um record
ex2 :: String
ex2 = "record { valor : 100 }"

--exemplo para acessar o valor
ex3 :: String
ex3 = "(record { x : 5 + 1 }).x"

-- record com varios campos
ex4 :: String
ex4 = "record { x : 10, y : 20 + 2, z : 5 * 5 }"

-- record com record
ex5 :: String
ex5 = "record { p : record { a : 1, b : 2 }, flag : true }"
