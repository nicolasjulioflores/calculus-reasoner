module Calculations where
import Lib
import Rewrites


-- Create calculation datatype 
data Calculation = Calc Expr [Step] deriving Eq
type Step = (LawName, Expr)

instance Show Calculation where
    showsPrec _ (Calc e steps)
        = showString "\n " .
          shows e .
          showString "\n" .
          foldr (\x r -> x . r) id (map showStep steps)

-- Shows the step when printing the output 
showStep :: Step -> ShowS
showStep (why,e)
    = showString "=   {" .
      showString why .
      showString "}\n  " .
      shows e .
      showChar '\n'

-- Calculates the law 
calculate :: [Claw] -> Expr -> Calculation
calculate claws e = Calc e (manyStep rws e)
    where rws e = [(name, e')
                  | Claw conds (Law name eqn) <- sortedLaws,
                    e' <- rewrites conds eqn e,
                    e' /= e]
          sortedLaws = claws

-- Handles the case of many steps 
manyStep :: (Expr -> [Step]) -> Expr -> [Step]
manyStep rws e
    = case rws e of 
        (step:_) -> (step:manyStep rws (snd step))
        [] -> []

