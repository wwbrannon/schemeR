## Allow writing R code in a prefix style, like Lisp

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
    if(is.expression(expr))
    {
        lst <- lapply(as.list(expr), infix)
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

        lst <- lapply(as.list(expr), infix)
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
