# calculus-reasoner

## Mar 5

For this submission, we would appreciate any feedback on how to improve our project such that we meet the requirements in the rubric. We aim do get as many of the 30 points as possible and would love to know what we are missing and how we might score at the moment. 

New ways to run our program
1. Added a test file that will run pre-determined formnulas. At the command line enter: `stack test`
2. Added a main file that will take user input, parse the input, then return the derivative. At the command line enter: `derive x x+2` for example. You must follow this format with `derive`, `your variable`, and `the equation`


## Mar 3
Our special extension is that we can reduce a wide range of expressions (using the Transform rules).
For example, all the examples that we have provided (run using the approach shown below) will be
reduced as much as they possibly can.

We'd like to get your feedback on anything that we should change before the final submission.

## Feb 27

We have included some more examples of problems we're able to solve. Use same method as below. 
We're working on implementing the arithmetic, and am trying to work out a way to clean up the types
for Substitution/Equation/Matches/Transform.

## Feb 25

The examples we have derived so far are in src/Lib.hs (see `examples`).

To run the example reasoning:

1. At the command line enter the ghci: `stack ghci`
2. Enter the command `map (calculate claws) examples`.

To run on your own example, you will have to use our datastructure as parsing is not yet setup. 
`calculate claws <your_example>`, replacing the part in `<>`.

