module Substitutions where
import Lib
import Data.Maybe (fromJust)

type Subst = [(Variable, Expr)]

emptySub = []
unitSub v e = [(v, e)]

-- apply
apply :: Subst -> Expr -> Expr
apply sub (Atom a) = applyA sub a
apply sub (Derive v e) = Derive v (apply sub e)
apply sub (Binary op e1 e2) = Binary op (apply sub e1) (apply sub e2)
apply sub (Unary op e) = Unary op (apply sub e)

applyA :: Subst -> Atom -> Expr
applyA sub (Var v) = binding sub v
applyA sub c = Atom c -- "Constants are preserved"

binding :: Subst -> Variable -> Expr
binding sub v = fromJust (lookup v sub)






-- [] is used as a Maybe type here
unify :: Subst -> Subst -> [Subst]
unify sub1 sub2 = if compatible sub1 sub2
                  then [union sub1 sub2]
                  else []

compatible :: Subst -> Subst -> Bool
compatible [] sub2 = True
compatible sub1 [] = True
compatible sub1@((v1,e1):sub1') sub2@((v2,e2):sub2')
    | v1<v2  = compatible sub1' sub2
    | v1==v2 = if e1==e2 then compatible sub1' sub2' else False
    | v1>v2  = compatible sub1 sub2'

union :: Subst -> Subst -> Subst
union [] sub2 = sub2
union sub1 [] = sub1
union sub1@((v1,e1):sub1') sub2@((v2,e2):sub2')
    | v1<v2  = (v1,e1):union sub1' sub2
    | v1==v2 = (v1,e1):union sub1' sub2'
    | v1>v2  = (v2,e2):union sub1  sub2'
                                                        
