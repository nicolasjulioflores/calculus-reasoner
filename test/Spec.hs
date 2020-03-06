
import Lib
import Calculations
import Parsing
import Text.Megaparsec


main :: IO ()

-- Allows us to test
main = do sequence $ map (eval) testExamples
          return ()

testExamples :: [String]
testExamples = [testEx1, testEx2, testEx3, testEx4, testEx5, testEx6]

testEx1 = "derive x x+1"
testEx2 = "derive x sin(x^2)"
testEx3 = "derive x x^3"
testEx4 = "derive x x^x"
testEx5 = "derive x 1 / (x^2)"
testEx6 = "derive x (cos(x)^2)"

eval :: String -> IO ()
eval s = case parseMaybe expr s of
            Just e -> print (calculate claws e)
            Nothing -> putStrLn "Parse error"


