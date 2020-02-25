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

data Claw = Claw Condition Law

data Law = Law LawName Equation
type LawName = String
type Equation = (Expr, Expr)
type Condition = Expr -> Bool

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
-- 1. d/dx (x + y)
ex :: Expr
ex = Derive "x" $ Binary "+" (Atom $ Var "x") (Atom $ Const 1.0)


-- 1. d/dx (sin (x ^ 2))
ex1 :: Expr
ex1 = Derive "x" $ Unary "sin" (Binary "pow" (Atom $ Var "x") (Atom $ Const 2.0))

-- 2. Applying chain rule: d/dx (x^2) * cos(x^2)
-- Note: that this exposes that we need to be careful with chain rule... 
-- apply derivative to the outer function while leaving the inner function untouched
ex2 :: Expr
ex2 = Binary "*" (Derive "x" $ Binary "pow" (Atom $ Var "x") (Atom $ Const 2.0)) (Unary "cos" $ Binary "pow" (Atom $ Var "x") (Atom $ Const 2.0))

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

pow_rule = Law "Derivative of pow"
                (Derive "x" $ Binary "pow" a b, 
                    Binary "*" (Binary "pow" a b) (Derive "x" (Binary "*" b (Unary "ln" a))))

self_rule = Law "Derivative of x" (Derive "x" $ Atom (Var "x"), Atom $ Const 1.0)

-- Pre-condition: isConstant a
-- isConstant :: Expr -> Bool
const_rule = Claw isConstant $ Law "Derivative of c" (Derive "x" a, Atom $ Const 0.0)

laws = [add_rule, sub_rule, prod_rule, quot_rule, sin_rule, cos_rule, ln_rule, pow_rule, self_rule]

claws = [const_rule] ++ map (Claw alwaysTrue) laws

-- Don't think we need anything below
{- Example Laws -}



-- 1. chain rule: d/dx (f(g(x)) = (d/dx g(x)) * d/dx(f(y)) where y = g(x) 
--  -> how do we express that inside is untouched? Need a way to express "derive this function?"
chain_rule = Law "Chain Rule" 
                    (Derive "x" $ Unary "f" $ Unary "g" (Atom $ Var "x"),
                     Binary "*" (Derive "x" $ Unary "g" (Atom $ Var "x")) (Unary "f'" $ Unary "g" (Atom $ Var "x")))

-- Rules: derivative for +, -, *, /, sin, cos, 
-- d/dx (a ^ b) = (a^b) * (d/dx (b * lna))

-- How to represent arbitrary constant? 2.0 in LHS should be constant
-- ex_law = Law "Derive (Constant)" (Derive "x" (Atom $ Const 2.0), Atom $ Const 0.0)

-- ex_law2 = Law "Derive (Constant * ?)" (Derive "x" $ (Binary "*" (Atom (c :: Float)) (e :: Expr)), Unary "f" (Atom $ Var "x"))
