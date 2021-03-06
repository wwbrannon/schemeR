
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://img.shields.io/travis/wwbrannon/schemeR.svg?style=flat)](https://travis-ci.org/wwbrannon/schemeR) [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/schemeR)](https://cran.r-project.org/package=schemeR) [![Downloads](https://cranlogs.r-pkg.org/badges/schemeR)](https://cran.r-project.org/package=schemeR) [![License](https://img.shields.io/:license-mit-blue.svg?style=flat)](https://wwbrannon.mit-license.org/)

schemeR
=======

The schemeR package provides a Lisp-like mini-language within R, with a prefix syntax and versions of many of the usual Lisp operators (let, cond, do, functional programming utilities, etc.). Code written with this package is still valid R, but the differences from what R code normally looks like highlight the language's roots in Scheme.

Real Lisp macros (the "non-hygienic", defmacro-based variety) are also supported. The implementation hews more closely to the way macros work in Common Lisp than other macro implementations for R, such as the one in the gtools package. Future work on schemeR will include an exploration of hygienic macros and whether they can be implemented as well.

Basic usage
-----------

The main entry point is the `schemeR()` function, which takes code written in this package's prefix style, translates it to the usual R syntax, and executes it.

For example, here are prefix and infix versions of a simple loop (the `invisible` is to hide final return values):

``` r
x <- sort(sample(1:10, 5, replace=TRUE))

# In the usual infix R syntax:
invisible(
for(i in x)
    print(1:i)
)
#> [1] 1
#> [1] 1 2
#> [1] 1 2 3 4
#> [1] 1 2 3 4 5 6 7 8
#>  [1]  1  2  3  4  5  6  7  8  9 10

#Equivalent, but prefix:
schemeR({
.(invisible,
  .(`for`, i, x,
    .(print, .(`:`, 1, i))))
})
#> [1] 1
#> [1] 1 2
#> [1] 1 2 3 4
#> [1] 1 2 3 4 5 6 7 8
#>  [1]  1  2  3  4  5  6  7  8  9 10
```

The `.` that appears in the prefix code is a function, which is how we can manage to write R this way and still have it parse. The unusual-for-Lisp syntax on display in prefix code is a consequence: lists must start with a `.(` rather than just a parenthesis, and elements are separated by commas rather than whitespace (because as far as R's parser is concerned, they're function arguments).

Despite its usefulness, the `.` function is just syntactic sugar: it generates a call to its first argument with all of its subsequent arguments, and requires a preprocessing step to be turned back into usable code. `schemeR()` does that preprocessing, with a few additional pieces of syntactic sugar, and executes the code that results.

Syntactic sugar
---------------

`schemeR()` understands a few pieces of infix syntactic sugar:

-   Code between `R(` and the matching `)` will be executed as-is, rather than being transformed back to the usual R syntax first;
-   Lists which open with `.b(` rather than `.(` are implicitly quasiquoted, just like the backtick in Lisp and Scheme. Because they're transformed into calls to `quasiquote` (see the section on macros below), its `.c()` and `.s()` notation for unquote and unquote-splicing can be used.

Lisp operators and other functions
----------------------------------

A variety of Lisp operators are also included, a few of which are just syntactically convenient aliases for things in base R (to avoid backtick-quoting, so that for example one can write `progn` instead of `` `{` ``). Some of the included operators are:

-   Lexical-binding constructs: let, let\* and letrec for granular control over variable scope;
-   Flow-control and conditional operators: do, cond, case, when and unless, and short-circuit 'and' and 'or';
-   List- and vector-manipulation functions: car, cdr, many of their compositions (e.g., caadr), and other list-processing utilities;
-   Functional-programming utilities: currying, function composition, Python's zip, Scheme's for-each, and others;
-   Misc: multiple-argument versions of common comparison and logical operators, assignment operators that simulate in-place modification for lists and vectors, and more.

Macros
------

This package's implementation of macros is closer to Common Lisp's version than other R implementations are. Specifically,

-   There's a version of the Lisp quasiquote that (unlike base R's `bquote`) provides both unquote and unquote-splicing. Expressions to unquote are surrounded by `.c()` (for "comma", as in Lisp) and expressions to unquote-splice are surrounded by `.s()` (for "splice"). `bquote` provides only `.()`, which is the equivalent of `.c()`.
-   `gensym`, as in Common Lisp, builds temporary symbols which macros can use to avoid shadowing variables in the containing scope;
-   To make both of these easier to use, macros expand in a way close to the way Lisp interpreters expand them. Conceptually, a Lisp macro is a function that computes its expansion, which is executed as if it had occurred in place of the macro call. This package's macros work exactly that way, rather than using `substitute()` to automatically interpolate parameters into their body expressions. The greater control over the macro's expansion makes it easier to write in an idiomatic Lisp style, and in particular allows for finer control of quoting and the use of `gensym`.

A working example of using macros demonstrates most package features. The macro below implements Python's list comprehensions:

``` r
schemeR({
.(defmacro, list.of, .(exp, s1, var, s2, lst, s3, test),
    .(cond, .(.(missing, test), .(set, test, TRUE))),
    .(let, .(.(result, .(gensym, "G1")), .(lvar, .(gensym, "G2"))),
        .b(.(let, .(.(.c(lvar), .c(lst))),
                .(do, .(.(.c(result), nil, .(cond, .(.c(test), .(append, .c(result), .c(exp))),
                                                   .(TRUE, .c(result)))),
                        .(.c(var), car(.c(lvar)), car(.c(lvar)))),
                      .(.(is.nil, .c(lvar)), .c(result)),
                      .(sset, .c(lvar), .(cdr, .c(lvar))))))))
})
#> list.of

# List comprehensions!
list.of(x^2, `for`, x, `in`, 1:10)
#>  [1]   1   4   9  16  25  36  49  64  81 100
list.of(x^2, `for`, x, `in`, 1:10, `if`, x >= 5)
#> [1]  25  36  49  64  81 100

# But they look a bit more natural in prefix form:
schemeR(
    .(list.of, x^2, `for`, x, `in`, 1:10)
)
#>  [1]   1   4   9  16  25  36  49  64  81 100

schemeR(
    .(list.of, x^2, `for`, x, `in`, 1:10, `if`, .(`>=`, x, 5))
)
#> [1]  25  36  49  64  81 100

# You have to backtick-quote "for", "in" and "if" because they're R reserved
# words, but you're free to use syntactic names if you want, because the macro
# discards those arguments anyway:
schemeR(
    .(list.of, x^2, with, x, of, 1:10)
)
#>  [1]   1   4   9  16  25  36  49  64  81 100

schemeR(
    .(list.of, x^2, with, x, of, 1:10, where, .(`>=`, x, 5))
)
#> [1]  25  36  49  64  81 100
```

Installation
------------

Install the released version from CRAN:

    install.packages("schemeR")

Install the dev version from github:

    install.packages("devtools")
    devtools::install_github("wwbrannon/schemeR")
