module Rewrites where
import Lib
import Substitutions
import Matches


match' :: Equation -> Expr -> [Expr]
match' (lhs, rhs) e =
    if (not . null) sub
    then [apply sub rhs]
    else []
        where sub = mustMatch lhs e

-- Try to match' top-level then try to match' inner
rewrites :: Equation -> Expr -> [Expr]
rewrites eqn expr@(Derive v e) 
    = match' eqn expr ++ 
      [Derive v e' | e' <- rewrites eqn e]
rewrites eqn expr@(Binary op e1 e2)
    = match' eqn expr ++ 
      [Binary op e1' e2 | e1' <- rewrites eqn e1] ++ 
      [Binary op e1 e2' | e2' <- rewrites eqn e2]
rewrites eqn expr@(Unary op e)
    = match' eqn expr ++ 
      [Unary op e' | e' <- rewrites eqn e]
rewrites eqn expr@(Atom _)
    = match' eqn expr
