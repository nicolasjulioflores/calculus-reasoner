module Main where

import Lib
import Calculations
import Parsing
import Text.Megaparsec

main :: IO ()
main = do l <- getLine
          e <- parseTest expr l
          putStrLn (show e)
