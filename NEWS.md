## schemeR 0.1.0
First release.

This version includes:

* A Lisp-like prefix syntax for writing R code and functions to translate in either direction between the prefix notation and the usual R syntax. Some syntactic sugar duplicates the backtick quote used in Lisp dialects.
* Support for writing Common Lisp-style macros, with defmacro, quasiquote and gensym. The macro semantics follow Common Lisp more closely than other macro implementations for R.
* Various functional programming operators not part of base R: currying, composition, partial function application, and versions of operators like zip and for-each that are part of other languages.
* Several Scheme flow-control operators: cond, case, do, and, or, when and unless.
* Lexical-binding or block-structure constructs from Scheme: let, let* and letrec.
* Miscellaneous other functions from Scheme: car, cdr and other list-manipulation functions; numeric and comparison operators; assignment macros; and others.
