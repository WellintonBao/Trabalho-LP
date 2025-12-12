module Interpreter where 

import Lexer 
import Parser 


recordError :: a
recordError = error "Erro: operação inválida em registro."

findFieldValue :: String -> [Expr] -> Maybe Expr
findFieldValue targetName [] = Nothing
findFieldValue targetName (RecordField (Var fieldName) val : rest)
    | targetName == fieldName = Just val
    | otherwise               = findFieldValue targetName rest
findFieldValue _ _ = Nothing 

isValue :: Expr -> Bool 
isValue BTrue  = True 
isValue BFalse = True 
isValue (Record fields) = all isValue fields
isValue (RecordField _ val) = isValue val 
isValue (Num _) = True 
isValue (Var _) = False
isValue (Lam _ _ _) = True 



isValue _ = False

subst :: String -> Expr -> Expr -> Expr 
subst x s y@(Var v) = if x == v then 
                        s 
                      else 
                        y 
subst x s (Num n) = (Num n)
subst x s BTrue = BTrue 
subst x s BFalse = BFalse 
subst x s (Lam y tp t1) = Lam y tp (subst x s t1)
subst x s (App t1 t2) = App (subst x s t1) (subst x s t2) 
subst x s (Add t1 t2) = Add (subst x s t1) (subst x s t2) 
subst x s (And t1 t2) = And (subst x s t1) (subst x s t2)

subst x s (Attri t1 t2) = Attri (subst x s t1) (subst x s t2)
subst x s (AccessRecord t1 t2) = AccessRecord (subst x s t1) (subst x s t2)
subst x s (Record fields) = Record (map (subst x s) fields)
subst x s (RecordField t1 t2) = RecordField (subst x s t1) (subst x s t2)
subst x s (If t1 t2 t3) = If (subst x s t1) (subst x s t2) (subst x s t3)
subst x s (Times t1 t2) = Times (subst x s t1) (subst x s t2)
subst x s (Eq t1 t2) = Eq (subst x s t1) (subst x s t2)
subst x s (Neq t1 t2) = Neq (subst x s t1) (subst x s t2)
subst x s (Or t1 t2) = Or (subst x s t1) (subst x s t2)
subst x s (Paren t1) = Paren (subst x s t1)

step :: Expr -> Expr 
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n1) e2) = let e2' = step e2
                           in Add (Num n1) e2' 
step (Add e1 e2) = Add (step e1) e2 


step (Times (Num n1) (Num n2)) = Num (n1 * n2) 
step (Times (Num n1) e2) = let e2' = step e2 in Times (Num n1) e2'
step (Times e1 e2) = Times (step e1) e2

step (Eq (Num n1) (Num n2)) = if n1 == n2 then BTrue else BFalse 
step (Eq (Num n1) e2) = let e2' = step e2 in Eq (Num n1) e2'
step (Eq e1 e2) = Eq (step e1) e2

step (Neq (Num n1) (Num n2)) = if n1 /= n2 then BTrue else BFalse
step (Neq (Num n1) e2) = let e2' = step e2 in Neq (Num n1) e2'
step (Neq e1 e2) = Neq (step e1) e2

step (And BFalse e2) = BFalse 
step (And BTrue e2) = e2 
step (And e1 e2) = And (step e1) e2 

step (Or BTrue e2) = BTrue 
step (Or BFalse e2) = e2
step (Or e1 e2) = Or (step e1) e2

step (If BTrue e1 e2) = e1 
step (If BFalse e1 e2) = e2
step (If e e1 e2) = If (step e) e1 e2

step (Paren t1) = t1

step (App (Lam x tp e1) e2) = if (isValue e2) then 
                                subst x e2 e1 
                              else 
                                App (Lam x tp e1) (step e2)

-- registro
step (Attri t1 t2)
    | not (isValue t2) = Attri t1 (step t2)
    | otherwise        = t2  

step (RecordField key val)
    | not (isValue val) = RecordField key (step val)
    | otherwise         = RecordField key val

step (Record fields) =
    case break (not . isValue) fields of
        (_, []) -> Record fields             
        (before, f:after) -> Record (before ++ [step f] ++ after)


step (AccessRecord rec key)
    | not (isValue rec) = AccessRecord (step rec) key


step (AccessRecord (Record fields) (Var name)) =
    case findFieldValue name fields of
        Just v  -> v
        Nothing -> recordError
step (AccessRecord _ _) = recordError


step e = error ("Stuck: no rule to step on " ++ show e)

eval :: Expr -> Expr
eval e = if isValue e then 
           e
         else 
           eval (step e)