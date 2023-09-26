## Haskulator
Shallow embedding style arithmetic expressions calculator. Numeric data are represented by Double datatype
```
expr := floating_point_number_literal | '(' expr ')' | expr operator expr
operator := '+' | '/' | '*' | '-'
```


## Stochastic bin packing solver
An algorithm which for a given list of numbers and a bin count can
find grouping of items such that their summ is approximately close in sublinear time.
I wrote it for my friend who needed to group test by execution time to feed them to
core bind executors.

## Prefix compression tree
Effectively for seqeunce which share long common prefixes this data structure alows
to quickly determine if a string is included in the set or not. Time-complexity is sublinear.