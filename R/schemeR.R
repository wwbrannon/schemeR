#' schemeR: Ports of certain Scheme facilities to R
#'
#' The schemeR package provides R versions of certain language features found
#' in "proper" Scheme dialects, insofar as this is possible without modifying
#' R itself. In particular, two categories of features are supported: a prefix
#' syntax for R code, like the \code{(car (cdr (cddr x)))} syntax in Lisp
#' dialects, and a way to write Lisp-like macros that are optimized to work
#' with this sort of prefix-formatted code.
#'
#' @section Note:
#' Because R is a Scheme dialect under the hood, prefix-formatted R is still
#' valid R code and parses with the built-in R parser. All of R's rules about
#' syntactic names (see \code{\link{make.names}}) still apply, and these rules
#' are stricter than is usual in Scheme.
#'
#' @section Limitations:
#' Because our prefix syntax is just a preprocessing step (R code expressed
#' with the \code{\link{.}}) function is transformed into its usual infix
#' form before evaluation), R's function-call mechanism is still used exactly
#' as-is. In particular, tail call elimination is still not possible, though
#' it may be added to future versions of this package as something the user
#' can request for prefix functions.
#'
#' @docType package
#' @name schemeR
NULL

#' @rdname schemeR
#' @export
schemeR <-
function(expr)
{
    expr <- substitute(expr)

    eval(infix(expr))
}
