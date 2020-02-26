module Rewrites where
import Lib
import Substitutions
import Matches

c_match :: Bool -> [Expr] -> [Expr]
c_match b es = if b then es else []

match' :: [Condition] -> Equation -> Expr -> [Expr]
match' conds (lhs, rhs) e =
    if (not . null) sub
    then [apply sub rhs]
    else []
        where sub = mustMatch conds lhs e

-- Try to match' top-level then try to match' inner
rewrites :: [Condition] -> Equation -> Expr -> [Expr]
rewrites conds eqn expr@(Derive v e) 
    = match' conds eqn expr ++
    -- = c_match (cond expr) (match' eqn expr) ++ 
      [Derive v e' | e' <- rewrites conds eqn e]
rewrites conds eqn expr@(Binary op e1 e2)
    = match' conds eqn expr ++
    -- = c_match (cond expr) (match' eqn expr) ++ 
      [Binary op e1' e2 | e1' <- rewrites conds eqn e1] ++ 
      [Binary op e1 e2' | e2' <- rewrites conds eqn e2]
rewrites conds eqn expr@(Unary op e)
    = match' conds eqn expr ++
    -- = c_match (cond expr) (match' eqn expr) ++ 
      [Unary op e' | e' <- rewrites conds eqn e]
rewrites conds eqn expr@(Atom _)
    = match' conds eqn expr
    -- = c_match (cond expr) (match' eqn expr)
