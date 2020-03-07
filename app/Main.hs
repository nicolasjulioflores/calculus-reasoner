module Main where

import Lib
import Calculations
import Parsing
import Text.Megaparsec


-- Takes the user input, parses the input, then it calculates the derivative 

-- Runs the main
-- Asks for user input 
main :: IO ()
main = do putStrLn "Enter expression: " 
          l <- getLine
          _ <- eval l
          return ()

-- parses the string and calculates the derivative
-- if it cannot parse, it will return "Parse error"
eval :: String -> IO ()
eval s = case parseMaybe expr s of
            Just e -> print (calculate claws e)
            Nothing -> putStrLn "Parse error"
