# A library to generate C code for regular expressions

This Haskell library performs code generation from a regular expression. It
basically "bakes in" the NFA denoted by the regular expression into C code. It
mostly follows the approach [outlined
here](https://swtch.com/~rsc/regexp/regexp2.html).

Efficiency of either the library itself or the generated code is not a concern.

This is a re-implementation of my earlier closed-source work in a related area.
