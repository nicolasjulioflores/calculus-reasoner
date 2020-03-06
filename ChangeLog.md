# Changelog for calculus-reasoner

## Mar 5
Changes:
1.  Updated Parser to account for derive 
2. Created a main function such that `stack run` works
3. Created a test function such that `stack test` works 

New ways to run our program
1. Added a test file that will run pre-determined formnulas. At the command line enter: `stack test`
2. Added a main file that will take user input, parse the input, then return the derivative. At the command line enter: `derive x x+2` for example. You must follow this format with `derive`, `your variable`, and `the equation`


## Mar 3
Changes: 
1. Added the special extension of a Transform rule that can reduce a wide range of expression (Added grouping like terms together)
2. Created a parser to parse the strings 
3. Updated Cabal 

Our special extension is that we can reduce a wide range of expressions (using the Transform rules).
For example, all the examples that we have provided (run using the approach shown below) will be
reduced as much as they possibly can.


## Feb 27
Changes:
1. Added new laws in the Lib.hs
2. Added support for the transform expression
3. Added conditional rules for basic arithmetic 
4. Cleaned up code 

We have included some more examples of problems we're able to solve. Use same method as below. 
We're working on implementing the arithmetic, and am trying to work out a way to clean up the types
for Substitution/Equation/Matches/Transform.

## Feb 25
Changes:
1. Added printing code and conditional laws
2. stack GHCI now runs on our command line 
The examples we have derived so far are in src/Lib.hs (see `examples`).

To run the example reasoning:
1. At the command line enter the ghci: `stack ghci`
2. Enter the command `map (calculate claws) examples`.

To run on your own example, you will have to use our datastructure as parsing is not yet setup. 
`calculate claws <your_example>`, replacing the part in `<>`.

