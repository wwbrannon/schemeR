#FIXME implement REST arguments for macros
#FIXME the macro needs to call eval() with envir=parent.frame(), which
#complicates a lot of things

#' Lisp macros for R
#'
#' These two functions create Lisp-style macros. \code{macro} creates and
#' returns anonymous macros; defmacro is a convenience wrapper around
#' \code{macro} which creates a macro and assigns it to a name in the current
#' environment. A full discussion of macros and how to use them is beyond the
#' scope of this page; see the vignettes for an R-focused introduction.
#'
#' Even though R is descended from Scheme, the macros implemented here are
#' based on the "non-hygienic" ones used in Common Lisp. They contrast with
#' the hygienic form that's traditional in Scheme, which is based on syntax
#' objects and the syntax-case special form.
#'
#' @param nm The symbol to which \code{defmacro} should bind the generated
#' macro.
#' @param params A parameter list, which when coerced to pairlist is acceptable
#' to the "function" constructor function.
#' @param ... Body statements.
#'
#' @return \code{macro} returns the created macro, which is an R function.
#' \code{defmacro}, as in Common Lisp, returns the symbol it's bound the new
#' macro to.
#'
#' @seealso
#' The \code{\link{gensym}} function, which generates temporary unique symbols
#' that macro definitions can use to avoid capturing variables from the calling
#' environment.
#'
#' @rdname macro
#' @name macro
#' @export
defmacro <-
function(nm, params, ...)
{
    target <- as.symbol(deparse(substitute(nm)))
    body <- eval(substitute(alist(...)))

    mac <- do.call(macro, c(list(params), body))
    expr <- bquote(.(target) <- .(mac))

    eval(expr, envir=parent.frame())

    return(target)
}

#' @rdname macro
#' @export
macro <-
function(params, ...)
{
    args <- eval(substitute(alist(...)))

    if(length(args) == 0)
        stop("Too few arguments to macro")

    #Put the body together; the hard part of doing this is
    #the need for the macro to substitute all of its args
    body <- list(as.symbol("{"))
    for(p in names(params))
        body <- c(body, bquote(.(as.symbol(p)) <- substitute(.(as.symbol(p)))))

    payload <- as.call(c(list(as.symbol("{")), args))
    s <- bquote(eval(.(payload), envir=environment()))
    body <- c(body, s)

    #Finally, roll it all together
    fn <- eval(call("function", as.pairlist(params), as.call(body)),
               envir=parent.frame())

    #Setting the environment here ensures that, as in lambda(), this function
    #closes over variables in the environment where the macro was defined; free
    #variables in body will, in other words, be resolved against the
    #environment the body statements came from
    environment(fn) <- parent.frame()

    fn
}

#' Build a temporary symbol
#'
#' \code{gensym} is an analogue of Common Lisp's gensym, for use in writing
#' macros. It builds randomly generated symbols which can be made to avoid
#' shadowing bindings already defined in an environment.
#'
#' \code{gensym} provides some additional control over the form of the symbol
#' it generates: the user can specify how long the symbol should be (though
#' asking for length-1 unique symbols is unlikely to be useful), specify a list
#' of symbols which should be discarded in favor of a new candidate if they are
#' generated, and ask for a particular string to be prepended for ease of
#' processing or as even more insurance against name conflicts.
#'
#' @param str A string to prepend to the generated symbol name.
#' @param lst A list or vector of symbols or strings. These values are
#' guaranteed not to be returned as the generated symbol.
#' @param len How long (in characters) the symbol should be.
#'
#' @return The generated symbol.
#'
#' @seealso
#' The main use of this function is providing temporary symbols that macro
#' definitions can use to avoid capturing variables from the calling
#' environment; see \code{\link{macro}} and \code{link{defmacro}} for the macro
#' facility in question.
#'
#' @export
gensym <-
function(str="G", lst=NULL, len=10)
{
    lst <- vapply(lst, as.character, character(1))
    nc <- nchar(str)

    if(length(str) > 1)
        stop("Too many prefixes specified")
    if(len <= 0)
        stop("Symbol length must be positive")
    if(nc >= len)
        stop("Prefix must have fewer than len characters")

    len <- len - nc

    nm <- paste0(str, gensym_candidate(len))
    if(!is.null(lst))
        repeat
        {
            #In principle this could run for an arbitrarily long time, but the
            #probability of going even 2 iterations is vanishingly small
            if(!(nm %in% lst))
                break

            nm <- paste0(str, gensym_candidate(len))
        }

    return(as.symbol(nm))
}

# Generate random character strings
#
# This function generates random character strings of a specified length.
# It's intended as a helper for gensym() in generating unique temporary
# symbols.
#
# @param len The length of the string to return.
#
# @return The generated character string
gensym_candidate <-
function(len)
{
    flchars <- c(letters, LETTERS)
    fl <- sample(flchars, 1)

    chars <- c(letters, LETTERS, vapply(0:9, as.character, character(1)))
    oc <- sample(chars, len - 1, replace=TRUE)

    nm <- paste0(c(fl, oc), collapse="")

    nm
}
