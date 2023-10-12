---
title: Week 10 Exercises
author: FIT2102 Programming Paradigms
margin: 1inch
---

## Requirements

Your quest this week is to embark on a coding journey and conquer the challenges presented by the exercises. Follow the instructions, ensuring that every code file is complete, and every test is conquered. Ensure all compilation warnings and errors are fixed, to ensure nothing stands in the way of your triumph.

May your code be resilient, your logic impeccable, and your debugging skills sharp. This quest is not merely about completion but about mastery. As you navigate through the exercises, tackle each challenge with determination and finesse.

We wish you the best of luck in completing this coding adventure. May your editor be swift, your compiler friendly, and your tests evergreen. Your success awaits at the end of this journey, where bug-free code and passing tests shall be your testament.

May the odds be ever in your favour.


`Instances.hs` will ask you to write `bind` and to reimplement Applicative and Functor instances for our now upgraded Parser using bind. 

`ParserExercises` will focus on creating a series of useful functions to help you with calculator.

`Helper.hs` contains a bunch of helper functions which might be useful to you along your quest. There are no questions to be completed with this file, but it will contains solutions from previous weeks, parsers which you have created before, to help you with the calculator parser. 

`Calculator.hs` will encompass the majority of work this week, where you will be creating a parser which can handle basic mathematical expressions. You will then use this parser for some interesting tasks. Firstly, using IO to evaluate all expressions in a file. Secondly, writing a REPL to handle user input and create an interesting program.

`FileIO` is an optional but very interesting exercise focusing on more FileIO operations. Here, you will be playing with a series of monadic operations to do interesting tasks.

## Some useful operators and functions

These are some useful operators and functions. If you don't know what they do, feel free to look them up on [Hoogle](https://hoogle.haskell.org).

```
Functor. Replacing or otherwise interacting with a parsed value.
- fmap, (<$>), ($>), (<$)

Applicative. Chaining together or using multiple Parser values.
- (*>), (<*), (<*>)
- pure, liftA2, liftA3, liftA4
- replicateM, traverse, sequence, replicate
- (<|>), some, many, foldr
```


## REPL

We will be building our first stand-alone Haskell app! The program will contain a REPL which will prompt the user for the filename.

The `main.hs` will contain a reference to the REPL function you will be writing in `Calculator.hs`

You build the app like this: `stack build`. Then to run it use `stack exec calculator`

To find where the app is built use `stack path --local-install-root`

This will print a directory, e.g., and to that directly append `/bin/calculate.exe` (without the .exe in Linux). 

Copying that file from that folder to `C:\Users\{YOUR_USERNAME}\AppData\Roaming\local\bin\printFiles.exe` will allow you to run it from anywhere:

```
calculator
```
So now you can make real programs in Haskell and you never need to use Python again.

Recommended order:
1. `Instances.hs`
2. `ParserExercises.hs`
3. `Calculator.hs`
4. (Optional side quest) `FileIO.hs`

