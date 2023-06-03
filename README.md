# Haskell-Calculator
This is a simple Turing-Complete calculator constructed by Haskell. This adapted from one of labs in the course CMSC 16100 at the University of Chicago. I have not taken the course personally; this is purely done through self-interests. 

To run the calculator, first compile the program through GHC, the run Main, which can be done through the following two commands: 

> ghc Main.hs

> ./Main

The calculator can handle infix operations like normal calculators. It also supports let expressions (can be used to define variables), if-then-else statements, and lambda expressions. Some example expressions would be: 

> let diff = (\x -> (\y -> y-x)) in diff 6 4

> 5 + let x = 6 in x

You can also perform recursion with this calculator. For example, the following expression evalute the 15th Fibonacci number

> let fib = (\x -> if x == 0 then 0 else if x == 1 then 1 else fib (x - 2) +  fib (x - 1)) in fib 15

For more information, consult [here] (http://cmsc-16100.cs.uchicago.edu/2021-autumn/Labs/posts/lab-6.html). 