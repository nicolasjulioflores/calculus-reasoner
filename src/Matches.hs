module Matches where
import Lib
import Substitutions

-- Calc (Derive "x" (Binary "+" (Atom (Var "x")) (Atom (Const 1.0)))) 
-- [("Derivative of (+)",Binary "+" (Derive "x" (Atom (Var "x"))) (Derive "x" (Atom (Const 1.0)))),
-- ("Derivative of x",Atom (Const 1.0))]


-- match LHS of an eqn to an expression
match :: Expr -> Expr -> [Subst]
-- Case: The LHS is an atom -> This can match any expression we have
-- match (Atom a) e = matchA (a, e)
-- ^ removing the above case only disallows rules that are only variables
-- that case seems pathological
-- Case: The top-level wrappers match -> Deconstruct the law (LHS) and continue matching
match e@(Derive _ _) e'@(Derive v1' e1') 
    = [mustMatch e e'] ++ match e e1'
match e@(Binary _ _ _) e'@(Binary op' e1' e2')
    = [mustMatch e e'] ++ match e e1' ++ match e e2'
match e@(Unary _ _) e'@(Unary op' e1') 
    = [mustMatch e e'] ++ match e e1'
-- Case: The top level wrappers don't match -> Try to match an inner expression
match e (Derive _ e1') = match e e1'
match e (Binary _ e1' e2') = match e e1' ++ match e e2'
match e (Unary _ e1') = match e e1'
match _ (Atom _) = emptySub

-- Once matching starts, it cannot be paused
-- This produces at most one full substitution
mustMatch :: Expr -> Expr -> Subst
mustMatch (Derive v1 e1) (Derive v1' e1')
    -- | compatible variable_sub sub = union variable_sub sub
    | compatible variable_sub sub = sub
    | otherwise                   = emptySub
       where 
           sub = mustMatch e1 e1' 
           variable_sub = unitSub v1 (Atom $ Var v1')
mustMatch (Binary op e1 e2) (Binary op' e1' e2')
    | op == op' = union (mustMatch e1 e1') (mustMatch e2 e2')
    | otherwise = emptySub
mustMatch (Unary op e1) (Unary op' e1')
    | op == op' = mustMatch e1 e1'
    | otherwise = emptySub
mustMatch (Atom a) e = matchA a e
mustMatch _ _ = emptySub


-- This function simply creates our substitution from our variable and any expression
matchA :: Atom -> Expr -> Subst
matchA (Var v) e = unitSub v e
-- matchA (Const ?)
