# calculus-reasoner

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

