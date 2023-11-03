---
title: Assignment 2 - Parser and Transpiler
author: FIT2102 Programming Paradigms
margin: 1inch
---

# Assignment 2: Parser and Transpiler

Please do not change the names of the functions defined in the Assignment.hs file. Each Part of the assignment has corresponding `parseExerciseX` and `prettyPrintExerciseX` that will parse and pretty print the input as per the requirements in that part.

You may (and are highly encouraged) to implement your parsers **alongside** these pre-defined functions.

## Running the Code

```
$ stack test
```

This will generate the transpiled JS files using the sample input JS files, by running your pretty printing function for each exercise.

## Running the Javascript Tests

In the javascript folder run:

```
$ npm i
$ npm run dev
```

All example scripts are stored within `javascript/inputs` and the output of your parser will be saved in `javascript/output`.

The tests on the page test:

- The generated code is valid JS (i.e. it runs without errors, including non-termination error)
- The generated code has certain properties of the original code (e.g. immutable variables are still immutable)
- The output is "prettified" from the input based on visual, side-by-side inspection
