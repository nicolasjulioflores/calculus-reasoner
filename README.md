# calculus-reasoner

Overview of Project:
1. Created a Calculus Homework solver that is an executable and is a command-line tool
    A calculus problem is input to our code and the output shows the intermediate steps
2. Implemented a special feature that can reduce a wide range of expressions using the transform rule 


How our code works: 

Ways to run our program
1. Added a test file that will run pre-determined formnulas. At the command line enter: `stack test`
2. Added a main file that will take user input, parse the input, then return the derivative. At the command line enter: `derive x x+2` for example. You must follow this format with `derive`, `your variable`, and `the equation`

Special Feature:

1. Our special extension is that we can reduce a wide range of expressions (using the Transform rules).
For example, all the examples that we have provided (run using the approach shown below) will be
reduced as much as they possibly can.



Relevant Files Contained in our submission

/src
1. Lib.hs - Stores all of our datatype, laws, rules, and our special feature 
2. Parsing.hs - Parses the data such that it correct incorrect spacings and returns in a format that matches our datatypes
3. Arithmetic.hs - Handles powers & arithmetic of powers 
4. Calculations.hs - Runs the calculation of the laws and shows the steps in output 
5. Matches.hs - Matches the condition with a substitution 
6. Rewrites.hs - Rewrites the expression after a match has been confirmed
7. Substitutions.hs - Applies the substitution

/app
1. main.hs - Takes user input formatted in a `derive variable exprssion` manner that will run the derivative. For example, the user would type in 
`derive x x^3` and the program will show how the end result is 3x^2. If there is an error or incorrect formatting, the program will return `Parse error`.
To utilize this application, type `stack run` in the command line 

/test
1. specs.hs - Runs preset tests that include 
                        testEx1 = "derive x x+1"
                        testEx2 = "derive x sin(x^2)"
                        testEx3 = "derive x x^3"
                        testEx4 = "derive x x^x"
                        testEx5 = "derive x 1 / (x^2)"
                        testEx6 = "derive x (cos(x)^2)"
To utilize this application, type `stack test` in the command line 


Others: 
1. Changelog.md - Details our changes throughout the process of making this project
2. README.md - Details our files within the code 
3. Calculus-reasoner.cabal - Details the library, test-suite, and executable of our code 

