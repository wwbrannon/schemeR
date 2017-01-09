## Allow writing R code in a prefix style, like Lisp

## Quoting is all handled in this file:
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

#' Facilities for writing R code in prefix notation
#'
#' \code{prefix} and \code{infix} convert R code between the usual infix
#' syntax and the Lisp-like prefix format written via the \code{.}
#' function (see below). A call to \code{prefix()} does not evaluate the passed
#' or the generated expressions, partly for efficiency reasons; \code{infix()},
#' though it still does not evaluate the generated expression, does do
#' partial evaluation of the passed expression: each call to \code{.} is
#' evaluated to the corresponding infix call.
#'
#' @param expr A prefix- or infix-formatted expression to convert. Code between
#' "R(" and ")" will not be converted.
#'
#' @return \code{prefix} and \code{infix} return the input expression
#' converted to prefix or infix.
#'
#' @examples
#' prefix(expression(x <- runif(10), y <- runif(10), cor(x, y)))
#'
#' prefix(expression(with(datasets::iris, sum(Sepal.Length^2 + Petal.Width))))
#'
#' @name prefix
#' @rdname prefix
#' @export
prefix <-
function(expr)
{
    if(is.expression(expr))
    {
        lst <- lapply(as.list(expr), prefix)
        return(as.expression(lst))
    } else if(is.call(expr))
    {
        if(expr[[1]] == as.symbol("R"))
        {
            if(length(expr) != 2)
            {
                stop("R() must have exactly one argument")
            }

            return(expr[[2]])
        }

        #Already prefix-ified code should be idempotent under this function
        if(expr[[1]] == as.symbol("."))
        {
            return(as.call(lapply(as.list(expr), prefix)))
        }

        lst <- lapply(as.list(expr), prefix)
        lst <- c(as.symbol("."), lst)

        return(as.call(lst))
    } else
    {
        return(expr)
    }
}

#' @examples
#'
#' infix(expression(.(`<-`, x, .(runif, 10)), .(`<-`, y, .(runif, 10)),
#'                  .(cor, x, y)))
#'
#' infix(expression(.(with, .(`::`, datasets, iris),
#'                              .(sum, .(`+`, .(`^`, Sepal.Length, 2),
#'                                            Petal.Width)))))
#' @rdname prefix
#' @export
infix <-
function(expr)
{
    s1 <- infix_transform_dots(expr)
    s2 <- infix_transform_quotes(s1)

    return(s2)
}

# Stage 2 of prefix=>infix processing
#
# This function takes the output of stage 1 (infix_transform_dots), which has
# had "." removed, and then handles a few additional pieces of syntactic
# sugar: ".q" is transformed into a call to base R's quote(), and .b is
# transformed into a call to our quasiquote(), which is like Scheme's
# quasiquote in that it handles by comma (written ".c" here) and comma-at
# (written ".s" for "splice"). The R() construction is again respected and
# expressions so escaped are not modified. (R's bquote is not quite powerful
# enough, so we had to roll our own quasiquote function.)
#
# @param expr The expression to process
#
# @return The transformed expression in infix syntax
infix_transform_quotes <-
function(expr)
{
    if(is.expression(expr))
    {
        lst <- lapply(as.list(expr), infix_transform_quotes)
        return(as.expression(lst))
    } else if(is.call(expr))
    {
        #We still need to avoid modifying this in stage 2
        if(expr[[1]] == as.symbol("R"))
        {
            if(length(expr) != 2)
            {
                stop("R() must have exactly one argument")
            }

            return(expr[[2]])
        } else if(expr[[1]] == as.symbol(".q"))
        {
            lst <- lapply(as.list(expr[-1]), infix_transform_quotes)
            return(as.call(list(as.symbol("quote"), lst)))
        } else if(expr[[1]] == as.symbol(".b"))
        {
            lst <- lapply(as.list(expr[-1]), infix_transform_quotes)
            return(as.call(list(as.symbol("quasiquote"), lst)))
        } else
        {
            lst <- lapply(as.list(expr), infix_transform_quotes)
            return(as.call(lst))
        }
    } else
    {
        return(expr)
    }
}

# Stage 1 of prefix=>infix processing
#
# This function takes an expression in full Lisp-like prefix syntax and does
# a few types of processing to bring it closer to the usual R infix syntax.
# First, calls to "." are evaluated and expand to match.call()[-1], but any
# occurrences of the R() construction are not modified.
#
# @param expr The expression to process
#
# @return The transformed expression in a form suitable for passing on to
# infix_remove_quotes
infix_transform_dots <-
function(expr)
{
    if(is.expression(expr))
    {
        lst <- lapply(as.list(expr), infix_transform_dots)
        return(as.expression(lst))
    } else if(is.call(expr))
    {
        if(expr[[1]] == as.symbol("R"))
        {
            if(length(expr) != 2)
            {
                stop("R() must have exactly one argument")
            }

            #Don't convert this - allow embedding R code in the usual infix
            #style in blocks of prefix code
            return(expr[[2]])
        }

        if(expr[[1]] == as.symbol("."))
        {
            env <- new.env(parent=emptyenv())
            assign(".", `.`, envir=env)

            expr <- eval(expr, envir=env)
        }

        lst <- lapply(as.list(expr), infix_transform_dots)
        return(as.call(lst))
    } else
    {
        return(expr)
    }
}

#' The \code{.} function allows writing an R function call in a Lisp-like prefix
#' format, rather than the f(x, y, ...) infix format that R usually uses.
#' When evaluated, a call to \code{.} doesn't actually execute the function
#' that's been expressed in prefix style, just generates the corresponding
#' infix call. It's intended as a notational aid, so that entire scripts
#' can be written in the prefix form.
#'
#' @param f The function that \code{.} should generate a call to.
#' @param ... Further arguments to the function f.
#'
#' @return \code{.} returns an unevaluated call to f with \code{...} arguments
#' carried through.
#'
#' @examples
#' .(`+`, 1, 2)
#'
#' .(sum, .(runif, 10), .(runif, 10))
#'
#' @rdname prefix
#' @export
. <-
function(f, ...)
{
    match.call()[-1]
}

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
