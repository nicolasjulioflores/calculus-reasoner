module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{- Datatypes -}
data Expr = Atom Atom
          | Unary String Expr
          | Binary String Expr Expr
          | Derive Variable Expr deriving Eq
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


-- Conditions:
alwaysTrue :: Expr -> Bool
alwaysTrue _ = True

isConstant :: Expr -> Bool
isConstant (Atom a) = case a of
                        Var _ -> False
                        Const _ -> True
isConstant (Unary _ e) = isConstant e
isConstant (Binary _ e1 e2) = (isConstant e1) && (isConstant e2)
isConstant (Derive _ e) = isConstant e


{- Examples -}
examples :: [Expr]
examples = [ex1, ex2, ex3, ex4]

-- 1. d/dx (x + 1)
ex1 = Derive "x" $ Binary "+" (Atom $ Var "x") (Atom $ Const 1.0)

-- 2. d/dx (sin (x ^ 2))
ex2 = Derive "x" $ Unary "sin" (Binary "^" (Atom $ Var "x") (Atom $ Const 2.0))

-- 3. d/dx (x ^ 3)
ex3 = Derive "x" $ Binary "^" (Atom $ Var "x") (Atom $ Const 3.0)

-- 4. d/dx (x ^ x)
ex4 = Derive "x" $ Binary "^" (Atom $ Var "x") (Atom $ Var "x")


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
                (Derive "x" $ Binary "/" a b, a) -- TODO

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

-- Pre-condition: isConstant a
-- isConstant :: Expr -> Bool
const_rule = Claw [("a", isConstant)] $ Law "Derivative of c" (Derive "x" a, Atom $ Const 0.0)

-- Lift +, *, -, / to apply to constant expressions
plus :: Expr -> Expr -> Expr
plus (Atom (Const f1)) (Atom (Const f2)) = Atom $ Const (f1 + f2)
-- mult (Atom (Const f1)) (Atom (Const f2)) = Atom $ Const (f1 + f2)
-- sub (Atom (Const f1)) (Atom (Const f2)) = Atom $ Const (f1 + f2)
-- plus (Atom (Const f1)) (Atom (Const f2)) = Atom $ Const (f1 + f2)

-- addition_rule = Claw ("a", isConstant) $ Law "Addition" (Binary "+" a b, a `plus` b)

laws = [add_rule, sub_rule, prod_rule, quot_rule, sin_rule, cos_rule, ln_rule, pow_rule, self_rule]

claws = [const_rule] ++ map (Claw []) laws
