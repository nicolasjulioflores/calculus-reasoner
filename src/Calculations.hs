module Calculations where
import Lib
import Rewrites

data Calculation = Calc Expr [Step] deriving Show
type Step = (LawName, Expr)

calculate :: [Law] -> Expr -> Calculation
calculate laws e = Calc e (manyStep rws e)
    where rws e = [(name, e')
                  | Law name eqn <- sortedLaws,
                    e' <- rewrites eqn e,
                    e' /= e]
          sortedLaws = laws

manyStep :: (Expr -> [Step]) -> Expr -> [Step]
manyStep rws e
    = case rws e of 
        (step:_) -> (step:manyStep rws (snd step))
        [] -> []

