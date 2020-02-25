module Rewrites where
import Lib
import Substitutions
import Matches

c_match :: Bool -> [Expr] -> [Expr]
c_match b es = if b then es else []

match' :: Equation -> Expr -> [Expr]
match' (lhs, rhs) e =
    if (not . null) sub
    then [apply sub rhs]
    else []
        where sub = mustMatch lhs e

-- Try to match' top-level then try to match' inner
rewrites :: Condition -> Equation -> Expr -> [Expr]
rewrites cond eqn expr@(Derive v e) 
    = c_match (cond expr) (match' eqn expr) ++ 
      [Derive v e' | e' <- rewrites cond eqn e]
rewrites cond eqn expr@(Binary op e1 e2)
    = c_match (cond expr) (match' eqn expr) ++ 
      [Binary op e1' e2 | e1' <- rewrites cond eqn e1] ++ 
      [Binary op e1 e2' | e2' <- rewrites cond eqn e2]
rewrites cond eqn expr@(Unary op e)
    = c_match (cond expr) (match' eqn expr) ++ 
      [Unary op e' | e' <- rewrites cond eqn e]
rewrites cond eqn expr@(Atom _)
    = c_match (cond expr) (match' eqn expr)
