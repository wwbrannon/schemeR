#' Lisp macros for R
#'
#' These two functions create Lisp-style macros. \code{macro} creates and
#' returns anonymous macros; defmacro is a convenience wrapper around
#' \code{macro} which creates a macro and assigns it to a name in the current
#' environment. A full discussion of macros and how to use them is beyond the
#' scope of this page; see the vignettes for an R-focused introduction.
#'
#' This section assumes some familiarity with macros. For an introduction, see
#' the package vignettes.
#'
#' FIXME
#'
#' Even though R is descended from Scheme, the macros implemented here are
#' based on the ones used in Common Lisp. They're referred to as "non-hygienic"
#' or defmacro-based, and contrast with the hygienic form that's traditional in
#' Scheme and is based on syntax objects and the syntax-case special form.
#'
#' @param nm The symbol to which \code{defmacro} should bind the generated
#' macro.
#' @param ... The infix form of prefix arguments.
#'
#' @return Macro returns the created macro, which is an R function. Defmacro,
#' as in Common Lisp, returns the symbol it's bound the new macro to.
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
function(nm, ...)
{
    args <- eval(substitute(alist(...)))

    #FIXME make sure the args are invariant under 2+ applications of
    #substitute
    mac <- do.call(macro, args)
    expr <- bquote(.(nm) <- .(mac))

    eval(expr, envir=parent.frame())

    return(nm)
}

#' @rdname macro
#' @export
macro <-
function(...)
{
    #Almost all of this for ~40 lines is exactly identical to lambda();
    #the difference is in the type of function that's constructed and
    #returned
    args <- eval(substitute(alist(...)))

    if(length(args) <= 1)
        stop("Too few arguments to macro")

    body <- as.call(c(list(as.symbol("{")), args[2:length(args)]))
    bd <- bquote({
        eval(.(body), envir=parent.frame())
    })

    #In this case and a few others we don't want to
    #take the infix form of a .(...) as a call, so let's
    #just turn it back into a list. This is a little gross,
    #but there's no good way to do it.
    params <- rapply(as.list(args[[1]]), as.list, how="replace")

    vals <- list()
    for(p in params)
    {
        if(!(length(p) %in% c(1,2)))
            stop("Invalid lambda argument list")

        nm <- as.character(p[[1]])
        if(length(p) == 1)
        {
            #See the long comment block in lambda() for how this works
            vals[[nm]] <- alist(x=)$x
        } else
        {
            vals[[nm]] <- p[[2]] #don't eval
        }
    }

    fn <- eval(call("function", as.pairlist(vals), bd), envir=parent.frame())

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
#' macros. It builds symbols which are guaranteed not to shadow or conflict
#' with bindings already defined in an environment.
#'
#' \code{gensym} provides some additional control over the form of the symbol
#' it generates: the user can specify how long the symbol should be (though
#' asking for length-1 unique symbols is unlikely to be useful), and ask for a
#' particular string to be prepended for ease of processing or as even more
#' insurance against name conflicts (perhaps relative to other environments).
#'
#' @param str A string to prepend to the generated symbol name.
#' @param envir The environment the symbol should be unique in.
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
function(str="G", envir=parent.frame(), len=10)
{
    nc <- nchar(str)

    if(length(str) > 1)
        stop("Too many prefixes specified")
    if(len <= 0)
        stop("Symbol length must be positive")
    if(nc >= len)
        stop("Prefix must have fewer than len characters")

    len <- len - nc

    lst <- as.list(envir)

    flchars <- c(letters, LETTERS)
    fl <- sample(flchars, 1)

    chars <- c(letters, LETTERS, vapply(0:9, as.character, character(1)))
    oc <- sample(chars, len - 1)

    nm <- paste0(c(fl, oc), collapse="")

    if(!is.null(lst))
    {
        repeat
        {
            if(!(nm %in% lst))
            {
                break
            }

            nm <- paste0(sample(chars, len), collapse="")
        }
    }

    return(as.symbol(paste0(str, nm)))
}
