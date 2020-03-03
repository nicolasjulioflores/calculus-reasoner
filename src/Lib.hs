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

-- RHS of Eqn should just be a Transform??
-- Transform should have a validator that checks that the substitution matches what it expects...
--
-- apply :: ExprBuilder -> Subst -> Expr
--
-- ExprBuilder (Subst -> Expr)

-- LHS of Eqn should be an expression matcher
-- Eqn (Variables, LHS, RHS)

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


-- Eqn :: Expr -> Expr
-- but the # of variables it accepts is parametric

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
isAZero (Atom (Const f)) = (f < delta) && (f > -delta)
    where delta = 0.00001
isAZero _ = False

isAOne :: Expr -> Bool
isAOne (Atom (Const f)) = (f - 1 < delta) && (f - 1 > -delta)
    where delta = 0.00001
isAOne _ = False


{- Examples -}
examples :: [Expr]
examples = [ex1, ex2, ex3, ex4, ex5]


ex_z = Unary "sin" (Unary "-" (Atom $ Const 0.0))
ex_v = Unary "sin" (Unary "-" (Atom $ Var "v"))

zlaws = [zero_law, sin_application]

zero_law = Claw [("a", isAZero)] $
                Law "-0 = 0" (Unary "-" a, Atom $ Const 0.0)

ex_sin = Unary "sin" (Binary "-" (Atom $ Const 1.0) (Atom $ Const 2.4))

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
a, b, c :: Expr -- Is this a good way to represent arbitrary expressions? These can be replaced by any expression
a = Atom $ Var "a"
b = Atom $ Var "b"
c = Atom $ Var "c"

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

-- Arithmetic laws
mult_div_rule = Law "a * (b / c) = (a * b) / c"
                    (Binary "*" a (Binary "/" b c), Binary "/" (Binary "*" a b) c)

double_pow_rule = Law "(a ^ b) ^ c = a ^ (b * c)"
                    (Binary "^" (Binary "^" a b) c, Binary "^" a (Binary "*" b c))

laws = [add_rule, sub_rule, prod_rule, quot_rule, sin_rule, cos_rule, ln_rule, pow_rule, self_rule] ++
        [mult_div_rule, double_pow_rule]

-- Arithmetic Claws
negate_rule = Claw [("a", isAZero)] $
                Law "0 - x = -x" (Binary "-" a b, Unary "-" b)

basic_claws = [const_rule, const_pow_rule, negate_rule]

-- Constant arithmetic rules:
add :: Transform
add [("a", (Atom (Const f1))), ("b", (Atom (Const f2)))]
    = Atom $ Const (f1 + f2)

addition_rule = Claw [("a", isAConstant), ("b", isAConstant)] $
                    Law "Addition" (Binary "+" a b, Transform add)

sub :: Transform
sub [("a", (Atom (Const f1))), ("b", (Atom (Const f2)))] 
    = Atom $ Const (f1 - f2)

subtraction_rule = Claw [("a", isAConstant), ("b", isAConstant)] $ 
                    Law "Subtraction" (Binary "-" a b, Transform sub)

mult :: Transform
mult [("a", (Atom (Const f1))), ("b", (Atom (Const f2)))]
    = Atom $ Const (f1 * f2)

multiplication_rule = Claw [("a", isAConstant), ("b", isAConstant)] $
                        Law "Multiplication" (Binary "*" a b, Transform mult)

divi :: Transform
divi [("a", (Atom (Const f1))), ("b", (Atom (Const f2)))]
    = Atom $ Const (f1 / f2)

division_rule = Claw [("a", isAConstant), ("b", isAConstant)] $ 
                    Law "Division" (Binary "/" a b, Transform divi)

-- Unary functions
sin' :: Transform
sin' [("a", (Atom (Const f)))]
    = Atom $ Const (sin f)

sin_application = Claw [("a", isAConstant)] $
                Law "Application of sin" (Unary "sin" a, Transform sin')

cos' :: Transform
cos' [("a", (Atom (Const f)))]
    = Atom $ Const (cos f)

cos_application = Claw [("a", isAConstant)] $
                Law "Application of cos" (Unary "cos" a, Transform cos')

-- Note: 'log x' returns natural log (http://zvon.org/other/haskell/Outputprelude/log_f.html)
log' :: Transform
log' [("a", (Atom (Const f)))]
    = Atom $ Const (log f)

log_application = Claw [("a", isAConstant)] $
                Law "Application of log" (Unary "log" a, Transform log')

-- Certain constants applied to variables
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
power_zero = Claw [("b", isAZero)] $
                Law "x ^ 0 = 1" (Binary "^" a b, Atom $ Const 1.0)
power_one = Claw [("b", isAOne)] $
                Law "x ^ 1 = x" (Binary "^" a b, a)

{- Combining like terms -}

-- Combine powers
powerGroupable :: Expr -> Bool
powerGroupable (Binary "*" (Binary "^" a c) (Binary "^" b d))
    = canGroupTwo a b
powerGroupable (Binary "*" (Binary "^" a c) b)
    = canGroupTwo a b
powerGroupable (Binary "*" a (Binary "^" b d))
    = canGroupTwo a b
powerGroupable (Binary "*" a b)
    = canGroupTwo a b
powerGroupable _ = False

canGroupTwo (Unary op e) (Unary op' e')
    = (op == op') && canGroupTwo e e'
canGroupTwo (Atom (Var a)) (Atom (Var b))
    = (a == b)
canGroupTwo _ _ = False

powerGroup :: Transform
powerGroup [("a", Binary "*" (Binary "^" a c) (Binary "^" b d))] 
    = Binary "^" a (Binary "+" c d)
powerGroup [("a", Binary "*" (Binary "^" a c) b)]
    = Binary "^" a (Binary "+" c (Atom $ Const 1.0))
powerGroup [("a", Binary "*" a (Binary "^" b d))]
    = Binary "^" a (Binary "+" d (Atom $ Const 1.0))
powerGroup [("a", Binary "*" a b)]
    = Binary "^" a (Atom $ Const 2.0)

power_group_law = Claw [("a", powerGroupable)] $ 
                    Law "(x ^ n) * (x ^ m) = x ^ (n + m)" (a, Transform powerGroup)

groupable :: Expr -> Bool
groupable (Binary "*" a b) = groupable' a b
groupable _ = False

groupable' :: Expr -> Expr -> Bool
groupable' (Binary "*" a c) b
    = (rearrangeable a b) || (rearrangeable c b) 
        || (groupable' a b) || (groupable' a c) || (groupable' b c)
groupable' a (Binary "*" b d)
    = (rearrangeable a b) || (rearrangeable a d)
        || (groupable' a b) || (groupable' a d) || (groupable' b d)
groupable' _ _ = False

-- NOTE: This is used as a base-case. We should move a multiplicative expression around if this
-- condition holds. Note that we need to separate this case out because we don't want to rearrange
-- Binary "*" (Binary "*" a c) b if the condition holds for a and c. Because this would mean the
-- rule would match expressions which DON'T need to be rearranged. Yet we still want to check 
-- the subtrees of a and c to check if they have expressions that would like to be rearranged.
rearrangeable a b
    = powerGroupable (Binary "*" a b) || (isAConstant a && isAConstant b)

group :: Transform
group [("a", (Binary "*" a b))] = group' a b

group' :: Expr -> Expr -> Expr
group' (Binary "*" a c) b
    = if rearrangeable a b then Binary "*" (Binary "*" a b) c
      else if rearrangeable c b then Binary "*" (Binary "*" c b) a
      else if groupable' a b then Binary "*" (group' a b) c
      else if groupable' a c then Binary "*" (group' a c) b
      else Binary "*" a (group' b c)
group' a (Binary "*" b d)
    = if rearrangeable a b then Binary "*" (Binary "*" a b) d
      else if rearrangeable a d then Binary "*" (Binary "*" a d) b 
      else if groupable' a b then Binary "*" (group' a b) d
      else if groupable' a d then Binary "*" (group' a d) b 
      else Binary "*" a (group' b d)

mult_group_law = Claw [("a", groupable)] $ 
                    Law "Rearranging terms (*)" (a, Transform group)

-- These laws should be applied last because they are mostly attempts to separate out terms so
-- they can be matched by other laws.
divisor_up = Claw [] $ 
                Law "a / b = a * (b ^ -1)" (Binary "/" a b, Binary "*" a (Binary "^" b (Atom $ Const (-1.0))))

unary_apply = Claw [] $ 
                Law "-a = -1 * a" (Unary "-" a, Binary "*" (Atom $ Const (-1.0)) a)


claws = basic_claws ++  
        map (Claw []) laws ++ 
        [subtraction_rule, addition_rule, multiplication_rule, division_rule] ++ 
        [sin_application, cos_application, log_application] ++ 
        [power_group_law, mult_group_law] ++ 
        [times_zero, times_zero', plus_zero, plus_zero', times_one, times_one', power_one, power_zero] ++ 
        [unary_apply, divisor_up]
