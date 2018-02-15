# R Utilities for Composing and Canonicalizing Functions

This package provides three functions, namely

1. `expression.simplify`, which attempts to resolve as many sub-expressions of an expression as possible,
2. `function.canonicalize`, which tries to resolve the promises and variables inside a function, and
3. `function.compose` which combines/chains two functions together.

The goal is to be able to produce readable and quick-to-evaluate functions,
in particular to be able to compose functions iteratively without creating
more and more complex entities that depend on different environments and are
largely opaque. With this package, you can retain readble functions with
as many variables substituted by constants and sub-expressions evaluated to
constants as possible, which should also speed up the computation of the
functions.
