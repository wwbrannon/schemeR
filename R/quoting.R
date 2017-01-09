## Various Lisp quote operators, some of which are provided already by R.

## Because normal Lisps have so much more permissive rules about what is and
## isn't a syntactic name, we can't use exactly the same names for quote,
## backquote, comma and comma-at. But it's extremely convenient to be able to
## refer to these things with short syntactic constructs, especially in macro
## definitions, so we'll give them alternate names:
##     quote ('): .q
##     backquote (`): .b
##     comma (,): .c
##     comma-at (,@): .s (for "splice")
## and use these names in lieu of the "." that otherwise begins all lists in
## our prefix syntax.
##
## Sharp-quote is unnecessary because, as in Scheme but contra Common Lisp,
## lambda is evaluable and evaluates directly to a function object.
##
## The upshot of all this is: .q is implemented with base R's quote function;
## backquote or quasiquote is defined below, and (as in Scheme) .c and .s are
## not procedures/special forms of their own, just syntax recognized by
## quasiquote.

#' Partial quoting in expressions
#'
#' A reimplementation of the backquote macro from Lisp/Scheme, with more
#' functionality than the base \code{bquote} function. quasiquote quotes
#' its argument, except for two kinds of user-requested evaluation and
#' interpolation. Adapted from base R's \code{bquote}.
#'
#' Unlike the bquote() found in base R, this version handles both of the types
#' of unquoting Lisp provides:
#' \itemize{
#' \item{terms wrapped in \code{.c()} are evaluated in the environment given
#' by \code{where}. This feature replicates Lisp's \code{unquote}, or the
#' comma notation, and behaves identically to \code{bquote}'s \code{.()}.}
#' \item{terms wrapped in \code{.s()} are evaluated in the environment given
#' by \code{where}, are assumed to evaluate to a list, pairlist or vector,
#' and its elements are spliced into the expression where the \code{.s()}
#' occurred. This feature replicates Lisp's \code{unquote-splicing}, or the
#' comma-at notation.}
#' }
#'
#' @section Warning:
#' Splicing unquote via \code{.s()} works on a purely lexical basis, and unlike
#' in Lisp, there is no guarantee the resulting object will make any sense.
#'
#' @param expr The expression to be partially quoted.
#' @param where The environment in which any evaluation should occur.
#'
#' @return The quoted expr, with partial evaluation and substitution done.
#'
#' @export
quasiquote <-
function(expr, where=parent.frame())
{
    expr <- substitute(expr)

    unquote <-
        function(e)
        {
            if (length(e) <= 1)
            {
                return(e)
            }
            else if (e[[1]] == as.symbol(".c") || e[[1]] == as.symbol(".s"))
            {
                #.c and .s are both handled identically here, but see below
                if(length(e) > 2)
                    stop(paste0("Too many arguments to ", as.character(e[[1]])))

                return(eval(e[[2]], envir=where))
            }
            else if(is.pairlist(e))
            {
                return(as.pairlist(lapply(e, unquote)))
            } else
            {
                alt <- list()
                ret <- lapply(e, unquote)

                for(i in seq_along(e))
                {
                    if(length(e[[i]]) > 1 && e[[i]][[1]] == ".s")
                    {
                        alt <- c(alt, ret[[i]])
                    } else
                    {
                        alt[[length(alt) + 1]] <- ret[[i]]
                    }
                }

                return(as.call(alt))
            }
        }

    unquote(expr)
}
