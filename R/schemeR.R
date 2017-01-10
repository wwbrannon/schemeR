#' schemeR: Make R more like Lisp/Scheme
#'
#' Provides a way to write R code in a Lisp-like \code{\link{prefix}} form,
#' with access to versions of many common Lisp operators (let, letrec, cond,
#' do, etc.). True (defmacro-based, rather than syntax-case) Lisp macros are
#' also supported and are optimized to work with this sort of prefix code.
#'
#' @section Interface functions:
#' The main entry point is the \code{schemeR()} function, which translates
#' prefix code into the usual infix R form, and evaluates it. Other functions
#' involved in implementing the prefix syntax are \code{prefix()} and
#' \code{infix()}, which respectively translate infix code \strong{to} prefix,
#' and translate prefix code \strong{to} infix. The \code{.} function is the
#' building block of prefix code, but is not especially useful in its own
#' right.
#'
#' @section Lisp operators:
#' A large number of Lisp operators have been implemented, mostly in ways that
#' are useful in either prefix or infix code. The highlight is the reasonably
#' complete \code{\link{macro}} support, but other operators run the gamut from
#' functional programming primitives (\code{lambda}, \code{zip},
#' \code{for.each}, etc.) to list/vector manipulation (\code{car}, \code{cdr},
#' \code{set.car}, etc.) to block-structure and lexical binding operators
#' (\code{let}, \code{letrec}, \code{cond}, etc.). See \code{\link{functions}}
#' for a full list and discussion.
#'
#' @docType package
#' @name schemeR-package
NULL

#' Write R code in a Lisp-like form
#'
#' schemeR() allows embedding Scheme-like code written in \code{\link{prefix}}
#' format, with the option of using various Lisp operators and features
#' including macros, into R code. The code in question is still syntactically
#' valid R, but has to be converted from \code{\link{prefix}} to
#' \code{\link{infix}} to be readily executable. Passing it to schemeR does the
#' conversion and evaluates the resulting 'traditional' R code, with some control
#' over the environment in which the evaluation is done.
#'
#' @param expr An expression, usually a block expression between "\{" and "\}",
#' intended to be in \code{\link{prefix}} form.
#'
#' @param pkg If \code{FALSE}, call \code{eval()} with its defaults,
#' \code{envir=parent.frame()} and \code{enclos=baseenv()}. If \code{TRUE},
#' use \code{envir=parent.frame()} but make an enclos environment containing
#' all objects from the package namespace environment and descending from
#' baseenv(). The motivation for this odd semantics is to allow expr to
#' refer to the many exported functions that are standard in Lisp/Scheme
#' (let, letrec, do, cond, etc.) without having to either use the \code{::}
#' operator (which goes against the spirit of Lisp-like prefix code) or
#' \code{require()} the package, as would, for example, be good practice
#' when developing another package.
#'
#' @return The result of evaluating \code{infix(expr)} in the specified
#' environment.
#'
#' @section Note:
#' Because R is a Scheme dialect under the hood, prefix or Lisp-like R is still
#' valid R code and parses with the built-in R parser. All of R's rules about
#' syntactic names (see \code{\link{make.names}}) still apply, and these rules
#' are stricter than usual among Lisps.
#'
#' @section Limitations:
#' Because our prefix syntax is just a preprocessing step (R code expressed
#' with the \code{\link{.}} function is transformed into its usual infix
#' form before evaluation), R's function-call mechanism is still used exactly
#' as-is. In particular, it's still not possible to do tail call elimination
#' safely for arbitrary functions.
#'
#' @examples
#' schemeR::schemeR({
#' .(let, .(.(x, .(c, 1, 3, 5, 7, 9))),
#'   .(do, .(.(x, .(c, 1, 3, 5, 7, 9), .(cdr, x)),
#'           .(s, 0, .(`+`, s, .(car, x))),
#'           .(foo, 4)),
#'     .(.(is.nil, x), s)))
#' }, pkg=TRUE) #=> 25
#'
#' require(schemeR)
#' schemeR({
#' .(define, x, .(sort, .(sample, .(`:`, 1, 10), 5, replace=TRUE)))
#' .(`for`, i, x, .(print, .(`:`, 1, i)))
#' })
#'
#' @export
schemeR <-
function(expr, pkg=FALSE)
{
    #TODO: we have to do recursive macro expansion here

    expr <- substitute(expr)

    envir <- parent.frame()
    if(!is.null(pkg) && pkg)
    {
        enclos <- new.env(parent=baseenv())
        lst <- as.list(getNamespace("schemeR"), all.names=TRUE)
        for(i in seq_along(lst))
        {
            nm <- names(lst)[i]
            assign(nm, lst[[nm]], envir=enclos)
        }
    }
    else
    {
        enclos <- baseenv()
    }

    eval(infix(expr), envir=envir, enclos=enclos)
}
