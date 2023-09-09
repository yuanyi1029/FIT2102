---
title: Week 7 Exercises
author: FIT2102 Programming Paradigms
margin: 1inch
---

## Requirements

Complete all the exercises as per the instructions found in the code files. All tests must pass. There must be no compilation warnings or errors.

Recommended order:

- `BinTree.hs`
- `Parser.hs`

## Parsing function type

The type of these functions are of the form

```haskell
String -> Maybe (String, a)
```

where `a` is the type of the value we are parsing into (also see https://tgdwyer.github.io/haskell2/#type-parameters-and-polymorphism).

> The order of the tuple `(String, a)` is not obvious at this time, but in later weeks we will see why this is useful.

- The `String` type values represent the input to be parsed, and
the remainder of the string, respectively.

Th- e `Maybe` type is used to represent the possibility of failure in parsing (also see https://tgdwyer.github.io/haskell2/#maybe).
