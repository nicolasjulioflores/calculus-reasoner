{-# OPTIONS_GHC -Wall #-}
-- Citation : https://markkarpov.com/tutorial/megaparsec.html
-- Citation: http://docs.restyled.io/restyler/parser-combinators-1.1.0/Control-Monad-Combinators-Expr.html


module Parser where


--import Control.Monad
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void(Void)
import Data.Functor.Identity (Identity)
import Lib


-- Create Type for Parser
type Parser = ParsecT Void String Identity


sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)


-- a parser for numbers 
pVariable :: Parser Atom
pVariable = Var <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

-- a parser for numbers 
pInteger :: Parser Atom
pInteger = Const <$> lexeme L.decimal
pAtom :: Parser Expr 
pAtom = Atom <$> pInteger <|> Atom <$> pVariable

-- a wrapper for lexemes that picks up all trailing white space using the supplied space consumer.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
-- a parser that matches given text using string internally and then similarly picks up all trailing white space.
symbol :: String -> Parser String
symbol = L.symbol sc

-- a parser to deal with parenthesis 
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

expr :: Parser Expr
expr = space*> (makeExprParser term table)

term :: ParsecT Void String Identity Expr
term = space*> (parens expr  <|> pAtom)

table :: [[Operator Parser Expr]]
table = [ [ prefix  "-"  
            , prefix  "+"   
            , prefix "cos"   
            , prefix "sin" 
            , prefix "ln" 
            ]
            , [ binary  "*" 
            , binary  "/" ]
            , [ binary  "+"
            , binary  "-"  ] 
            , [ binary "^" ] ]


binary :: String -> Operator (ParsecT Void String Identity) Expr
binary  name = InfixL (Binary <$> symbol name)

prefix :: String -> Operator (ParsecT Void String Identity) Expr
prefix  name = Prefix  (Unary <$> symbol name)


