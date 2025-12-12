module TypeChecker where 

import Lexer 
import Data.List (lookup) 

type Ctx = [(String, Ty)]

typeof :: Ctx -> Expr -> Maybe Ty 
typeof ctx BTrue = Just TBool 
typeof ctx BFalse = Just TBool 
typeof ctx (Num n) = Just TNum 
typeof ctx (Add e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TNum, Just TNum) -> Just TNum 
                           _                      -> Nothing

typeof ctx (Times e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                             (Just TNum, Just TNum) -> Just TNum
                             _                      -> Nothing

typeof ctx (And e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TBool, Just TBool) -> Just TBool 
                           _                        -> Nothing

typeof ctx (Or e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                          (Just TBool, Just TBool) -> Just TBool
                          _                        -> Nothing
                          
typeof ctx (Eq e1 e2) = case (typeof ctx e1, typeof ctx e2) of
    (Just t1, Just t2) | t1 == t2 && (t1 == TNum || t1 == TBool) -> Just TBool
    _ -> Nothing

typeof ctx (Neq e1 e2) = case (typeof ctx e1, typeof ctx e2) of
    (Just t1, Just t2) | t1 == t2 && (t1 == TNum || t1 == TBool) -> Just TBool
    _ -> Nothing
                           
typeof ctx (If e e1 e2) = case typeof ctx e of 
                            Just TBool -> case (typeof ctx e1, typeof ctx e2) of 
                                            (Just t1, Just t2) | t1 == t2  -> Just t1 
                                                               | otherwise -> Nothing 
                                            _ -> Nothing  
                            _ -> Nothing 

typeof ctx (Var x) = lookup x ctx 

typeof ctx (Paren e) = typeof ctx e

typeof ctx (Lam x tp b) = let ctx' = (x,tp) : ctx 
                            in case (typeof ctx' b) of 
                                 Just tr -> Just (TFun tp tr)
                                 _ -> Nothing 

typeof ctx (App e1 e2) = case typeof ctx e1 of 
                           Just (TFun tp tr) -> case typeof ctx e2 of 
                                                  Just t2 | t2 == tp -> Just tr 
                                                  _ -> Nothing 
                           _ -> Nothing 

typeof ctx (Attri _ e2) = typeof ctx e2 


typeof ctx (Record fields) =
    case typeofFields ctx fields of
        Just fieldTypes -> Just (TRecord fieldTypes)
        Nothing         -> Nothing

typeof ctx (AccessRecord e key) = 
    case typeof ctx e of 
        Just (TRecord fieldTypes) -> 
            case key of
                Var name -> lookup name fieldTypes
                _ -> Nothing
        _ -> Nothing        


typeofFields :: Ctx -> [Expr] -> Maybe [(String, Ty)]
typeofFields _ [] = Just []

typeofFields ctx (RecordField (Var name) valExp : rest) =
    case typeof ctx valExp of
        Just tVal ->
            case typeofFields ctx rest of
                Just tRest -> Just ((name, tVal) : tRest)
                _ -> Nothing
        _ -> Nothing

typeofFields _ _ = Nothing


typecheck :: Expr -> Expr 

typecheck e = case typeof [] e of 
                Just _ -> e 
                _      -> error "Type error!"