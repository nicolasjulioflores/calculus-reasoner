module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

    {- Datatypes -}
newtype Expr = Compose [Atom] deriving (Show, Eq)
data Atom = Var String 
          | Con String [Expr] deriving (Show, Eq)

data Law = Law String Equation
type Equation = (Expr, Expr)
