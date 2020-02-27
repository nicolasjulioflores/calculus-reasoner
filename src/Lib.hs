module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{- Datatypes -}
data Expr = Atom Atom
          | Unary String Expr
          | Binary String Expr Expr
          | Derive Variable Expr
          | Transform Transform
type Transform = ([(Variable, Expr)] -> Expr)

instance Eq Expr where
    (Atom a) == (Atom b)
        = a == b
    (Unary op e) == (Unary op' e') 
        = (op == op') && (e == e')
    (Binary op e1 e2) == (Binary op' e1' e2')
        = (op == op') && (e1 == e1') && (e2 == e2')
    (Derive _ e) == (Derive _ e')
        = (e == e')
    _ == _ = False -- Note: Transforms should never be compare for equality

data Atom = Var Variable 
          | Const Float deriving Eq
type Variable = String

data Claw = Claw [Condition] Law

data Law = Law LawName Equation
type LawName = String
type Equation = (Expr, Expr)
type Condition = (Variable, Expr -> Bool)

instance Show Atom where
    showsPrec _ (Var v) = showString v
    showsPrec _ (Const f) = showString (show f)

instance Show Expr where
    showsPrec _ (Atom a) = showsPrec 0 a
    showsPrec _ (Unary op e) 
      = showString op . showParen (True) (showsPrec 0 e)
    showsPrec _ (Derive v e)
      = showString "d/d" . showString v . showParen (True) (showsPrec 0 e)
    showsPrec p (Binary op e1 e2)
      = showParen (p == 1) (showsPrec 1 e1 . showString " " . showString op . showString " " . showsPrec 1 e2)


-- Transforms
-- sortMult :: Transform
-- sortMult vs = 

-- Lift +, *, -, / to apply to constant expressions
-- plus, mult, sub, divi :: Expr -> Expr -> Expr
-- plus (Atom (Const f1)) (Atom (Const f2)) = Atom $ Const (f1 + f2)
-- mult (Atom (Const f1)) (Atom (Const f2)) = Atom $ Const (f1 * f2)
-- sub (Atom (Const f1)) (Atom (Const f2)) = Atom $ Const (f1 - f2)
-- divi (Atom (Const f1)) (Atom (Const f2)) = Atom $ Const (f1 / f2)

-- Conditions:
alwaysTrue :: Expr -> Bool
alwaysTrue _ = True

isAVar :: Expr -> Bool
isAVar (Atom (Var _)) = True
isAVar _ = False

isConstant :: Expr -> Bool
isConstant (Atom a) = case a of
                        Var _ -> False
                        Const _ -> True
isConstant (Unary _ e) = isConstant e
isConstant (Binary _ e1 e2) = (isConstant e1) && (isConstant e2)
isConstant (Derive _ e) = isConstant e

isAConstant :: Expr -> Bool
isAConstant (Atom (Const _)) = True
isAConstant _ = False

isAZero :: Expr -> Bool
isAZero (Atom (Const f)) = (f < delta)
    where delta = 0.00001
isAZero _ = False

isAOne :: Expr -> Bool
isAOne (Atom (Const f)) = (f - 1 < delta)
    where delta = 0.00001
isAOne _ = False


{- Examples -}
examples :: [Expr]
examples = [ex1, ex2, ex3, ex4, ex5]

-- 1. d/dx (x + 1)
ex1 = Derive "x" $ Binary "+" (Atom $ Var "x") (Atom $ Const 1.0)

-- 2. d/dx (sin (x ^ 2))
ex2 = Derive "x" $ Unary "sin" (Binary "^" (Atom $ Var "x") (Atom $ Const 2.0))

-- 3. d/dx (x ^ 3)
ex3 = Derive "x" $ Binary "^" (Atom $ Var "x") (Atom $ Const 3.0)

-- 4. d/dx (x ^ x)
ex4 = Derive "x" $ Binary "^" (Atom $ Var "x") (Atom $ Var "x")

-- 5. d/dx (1 / (x ^ 2))
ex5 = Derive "x" $ Binary "/" (Atom $ Const 1.0) (Binary "^" (Atom $ Var "x") (Atom $ Const 2.0))

-- 6. d/dx (cos(x) ^ 2)
ex6 = Derive "x" $ Binary "^" (Unary "cos" (Atom $ Var "x")) (Atom $ Const 2.0)


{- Derivative Laws -}
a, b :: Expr -- Is this a good way to represent arbitrary expressions? These can be replaced by any expression
a = Atom $ Var "a"
b = Atom $ Var "b"

add_rule = Law "Derivative of (+)"
                (Derive "x" $ Binary "+" a b, Binary "+" (Derive "x" a) (Derive "x" b))

sub_rule = Law "Derivative of (-)"
                (Derive "x" $ Binary "-" a b, Binary "-" (Derive "x" a) (Derive "x" b))

prod_rule = Law "Derivative of (*)"
                (Derive "x" $ Binary "*" a b, 
                    Binary "+" (Binary "*" (Derive "x" a) b) (Binary "*" a (Derive "x" b)))

quot_rule = Law "Derivative of (/)"
                (Derive "x" $ Binary "/" a b, 
                    Binary "/" 
                        (Binary "-" (Binary "*" b (Derive "x" a)) (Binary "*" a (Derive "x" b)))
                        (Binary "^" b (Atom $ Const 2.0)))

sin_rule = Law "Derivative of sin"
                (Derive "x" $ Unary "sin" a, Binary "*" (Derive "x" a) (Unary "cos" a))

cos_rule = Law "Derivative of cos"
                (Derive "x" $ Unary "cos" a, Binary "*" (Derive "x" a) (Unary "-" (Unary "sin" a)))

ln_rule = Law "Derivative of ln"
                (Derive "x" $ Unary "ln" a, Binary "*" (Derive "x" a) (Binary "/" (Atom $ Const 1.0) a))

pow_rule = Law "Derivative of (^)"
                (Derive "x" $ Binary "^" a b, 
                    Binary "*" (Binary "^" a b) (Derive "x" (Binary "*" b (Unary "ln" a))))

self_rule = Law "Derivative of x" (Derive "x" $ Atom (Var "x"), Atom $ Const 1.0)

-- Claws:
const_rule = Claw [("a", isConstant)] $ Law "Derivative of c" (Derive "x" a, Atom $ Const 0.0)

const_pow_rule = Claw [("a", isAVar), ("b", isAConstant)] $
    Law "Derivative of x^p if p is a constant"
        (Derive "x" $ Binary "^" a b, 
            Binary "*" b (Binary "^" a (Binary "-" b (Atom $ Const 1.0))))

laws = [add_rule, sub_rule, prod_rule, quot_rule, sin_rule, cos_rule, ln_rule, pow_rule, self_rule]


sub :: Transform
sub [("a", (Atom (Const f1))), ("b", (Atom (Const f2)))] 
  = Atom $ Const (f1 - f2)

subtraction_rule = Claw [("a", isAConstant), ("b", isAConstant)] $ 
                    Law "Subtraction" (Binary "-" a b, Transform sub)

-- addition_rule = Claw [("a", isAConstant), ("b", isAConstant)] $ 
--                     Law "Addition" (Binary "+" a b, a `plus` b)
-- multiplication_rule = Claw [("a", isAConstant), ("b", isAConstant)] $ 
--                     Law "Multiplication" (Binary "*" a b, a `mult` b)
-- division_rule = Claw [("a", isAConstant), ("b", isAConstant)] $ 
--                     Law "Division" (Binary "/" a b, a `divi` b)

times_zero = Claw [("a", isAZero)] $
                Law "0 * x = 0" (Binary "*" a b, Atom $ Const 0.0)
times_zero' = Claw [("b", isAZero)] $
                Law "x * 0 = 0" (Binary "*" a b, Atom $ Const 0.0)
plus_zero = Claw [("a", isAZero)] $
                Law "0 + x = x" (Binary "+" a b, b)
plus_zero' = Claw [("b", isAZero)] $
                Law "x + 0 = x" (Binary "+" a b, a)
times_one = Claw [("a", isAOne)] $
                Law "1 * x = x" (Binary "*" a b, b)
times_one' = Claw [("b", isAOne)] $
                Law "x * 1 = x" (Binary "*" a b, a)

claws = [const_rule, const_pow_rule] ++ 
        [subtraction_rule] ++ 
        -- [addition_rule, subtraction_rule, multiplication_rule, division_rule] ++
        [times_zero, times_zero', plus_zero, plus_zero', times_one, times_one'] ++ 
        map (Claw []) laws
