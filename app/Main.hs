module Main where

import Lib
import Calculations
import Parsing
import Text.Megaparsec

main :: IO ()
main = do putStrLn "Enter expression: " 
          l <- getLine
          _ <- eval l
          return ()

eval :: String -> IO ()
eval s = case parseMaybe expr s of
            Just e -> print (calculate claws e)
            Nothing -> putStrLn "Parse error"
