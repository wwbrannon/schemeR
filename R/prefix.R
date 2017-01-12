#' Facilities for writing R code in prefix notation
#'
#' The \code{.} function allows writing an R function call in a Lisp-like
#' prefix format, rather than the Algol-like infix format that R usually
#' uses. \code{prefix} and \code{infix} convert R code between infix and
#' prefix.
#'
#' When evaluated, a call to \code{.} doesn't actually execute the function
#' that's been expressed in prefix style, just generates the corresponding
#' infix call. It's intended as a notational aid, so that entire scripts
#' can be written in the prefix form.
#'
#' A call to \code{prefix()} does not evaluate the passed
#' or the generated expressions, partly for efficiency reasons; \code{infix()},
#' though it still does not evaluate the generated expression, does do
#' partial evaluation of the passed expression: each call to \code{.} is
#' evaluated to the corresponding infix call.
#'
#' \code{infix} and \code{prefix} are not quite inverses of each other, because
#' \code{infix} understands certain pieces of infix syntactic sugar. Examples
#' include the ".q" and ".b" quoting abbreviations, and the lambda lists passed
#' to \code{lambda}, \code{macro} and \code{defmacro}.
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
        #Don't convert this - allow embedding R code in the usual infix
        #style in blocks of prefix code
        if(expr[[1]] == as.symbol("R"))
        {
            if(length(expr) != 2)
            {
                stop("R() must have exactly one argument")
            }

            return(expr[[2]])
        } else if(expr[[1]] == as.symbol("."))
        {
            #Most of what in Lisp are special forms can be handled in the usual
            #R way with substitute() and quoting; the exception is the
            #parameter lists given to lambda, macro and defmacro, when they
            #contain default values. If we don't handle these here, expressions
            #like
            #    .(lambda, .(n=.(curry, `+`, 4), m), .(n, m))
            #will not be turned into the correct infix form.
            if(expr[[2]] == as.symbol("lambda") ||
               expr[[2]] == as.symbol("macro"))
            {
                params <- as.list(expr[[3]])[-1]
                params <- lapply(params, infix_transform_dots)

                vals <- list()
                for(i in seq_along(params))
                {
                    if(is.null(names(params)))
                        nms <- c("")
                    else
                        nms <- names(params)

                    #passing "m" and "m=" are equivalent, but because R is
                    #terrible, are represented differently. this case is what
                    #we see when you pass "m" w/o the equals sign
                    if(nms[[i]] == "")
                    {
                        nm <- as.character(params[[i]])

                        #This one takes some explanation: in building a function by
                        #hand, the formal argument list is passed in to the "function"
                        #constructor as a pairlist, whose tags are used as the argument
                        #names. The values become the argument defaults. If you want an
                        #argument with no default, which will raise an error if missing,
                        #the value that has to be passed in is a zero-length symbol.
                        #There's no obvious way to generate one of these: as.symbol("") and
                        #related constructs all raise errors. But, as it turns out, alist()
                        #returns them for arguments that have a tag but no value. The fact
                        #that this little trick is possible saves us from having to write C
                        #to generate the necessary zero-length name.
                        vals[[nm]] <- alist(x=)$x
                    }
                    else
                    {
                        nm <- as.character(nms[[i]])
                        vals[[nm]] <- params[[i]] #don't eval
                    }
                }

                body <- lapply(as.list(expr[4:length(expr)]),
                               infix_transform_dots)

                return(as.call(c(expr[[2]], list(vals), body)))
            } else if(expr[[2]] == as.symbol("defmacro"))
            {
                name <- expr[[3]]

                params <- as.list(expr[[4]])[-1]
                params <- lapply(params, infix_transform_dots)

                vals <- list()
                for(i in seq_along(params))
                {
                    if(is.null(names(params)))
                        nms <- c("")
                    else
                        nms <- names(params)

                    if(names(params)[[i]] == "")
                    {
                        nm <- as.character(params[[i]])

                        vals[[nm]] <- alist(x=)$x
                    }
                    else
                    {
                        nm <- as.character(nms[[i]])
                        vals[[nm]] <- params[[i]]
                    }
                }

                body <- lapply(as.list(expr[5:length(expr)]),
                               infix_transform_dots)

                return(as.call(c(expr[[2]], name, list(vals), body)))
            } else
            {
                env <- new.env(parent=emptyenv())
                assign(".", `.`, envir=env)

                expr <- eval(expr, envir=env)
            }
        }

        #Not the else clause - run this regardless
        lst <- lapply(as.list(expr), infix_transform_dots)
        return(as.call(lst))
    } else
    {
        return(expr)
    }
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
