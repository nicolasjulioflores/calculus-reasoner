module Arithmetic where
import Lib

data Add  = Add [Mult]

data Mult = Mult [Term]

data Term = Atom Atom
          | Pow Term Term
          | Unary String Add -- One of these options should be "id?"

-- x ^ x
ex :: Add
ex = Add [Mult [Pow (Arithmetic.Atom $ Var "x") (Arithmetic.Atom $ Var "x")]]

-- d/dx (x ^ 3.0) = 
-- (x ^ 3.0) * ((0.0 * ln(x)) + (3.0 * (1.0 * (1.0 / x))))
ex1 :: Add
ex1 = Add [Mult [
                (Pow (Arithmetic.Atom $ Var "x") (Arithmetic.Atom $ Const 3.0)),
                (Arithmetic.Unary "id" $ Add [
                    (Mult [(Arithmetic.Atom $ Const 0.0), 
                           (Arithmetic.Unary "ln" $ Add [ Mult [ Arithmetic.Atom $ Var "x"]])
                          ]),
                    (Mult [(Arithmetic.Atom $ Const 3.0),
                           (Arithmetic.Atom $ Const 1.0),
                           (Arithmetic.Atom $ Const 1.0),
                           (Pow (Arithmetic.Atom $ Var "x") (Arithmetic.Atom $ Const 1.0))
                          ])
                                             ])
                ]]

-- Distributive #1
-- 1 * x, 0 + x -> If these items are in the list remove them
-- 0 * x -> If this item is in the list then replace it with: Mult [Arithmetic.Atom $ Const 0.0]


-- Sorting
-- Collect like terms and perform addition

