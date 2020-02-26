module Matches where
import Lib
import Substitutions

-- Once matching starts, it cannot be paused
-- This produces at most one full substitution
mustMatch :: [Condition] -> Expr -> Expr -> Subst
mustMatch conds (Derive v1 e1) (Derive v1' e1')
    -- | compatible variable_sub sub = union variable_sub sub
    | compatible variable_sub sub = sub
    | otherwise                   = emptySub
       where 
           sub = mustMatch conds e1 e1' 
           variable_sub = unitSub v1 (Atom $ Var v1')
mustMatch conds (Binary op e1 e2) (Binary op' e1' e2')
    | op == op' = union (mustMatch conds e1 e1') (mustMatch conds e2 e2')
    | otherwise = emptySub
mustMatch conds (Unary op e1) (Unary op' e1')
    | op == op' = mustMatch conds e1 e1'
    | otherwise = emptySub
mustMatch conds (Atom a) e = matchA conds a e
mustMatch _ _ _ = emptySub


-- This function simply creates our substitution from our variable and any expression
matchA :: [Condition] -> Atom -> Expr -> Subst
matchA conds (Var v) e 
  = if cond_holds then unitSub v e else emptySub
      where cond_holds = case lookup v conds of 
                           Just cond -> cond e
                           Nothing -> True
-- matchA (Const ?)
