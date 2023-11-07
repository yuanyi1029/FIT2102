---
title: Week 9 Exercises
author: FIT2102 Programming Paradigms
margin: 1inch
---

## Requirements

Complete all the exercises as per the instructions found in the code files. All tests must pass. There must be no compilation warnings or errors.

Recommended order:
1. `Folds.hs`
2. `Parser.hs`
3. `JSON.hs`

## Some useful operators and functions

These are some useful operators and functions. If you don't know what they do, feel free to look them up on [Hoogle](https://hoogle.haskell.org).

```
Functor. Replacing or otherwise interacting with a parsed value.
- fmap, (<$>), ($>), (<$)

Applicative. Chaining together or using multiple Parser values.
- (*>), (<*), (<*>)
- pure, liftA2, liftA3, liftA4
- replicateM, traverse, sequence, replicate
- (<|>), foldr
```
