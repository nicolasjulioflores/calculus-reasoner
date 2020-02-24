module Rewrites where
import Lib
import Substitutions
import Matches

rewrites :: Equation -> Expr -> [Expr]
rewrites (e1, e2) e = [apply sub e2 | sub <- match e1 e]

