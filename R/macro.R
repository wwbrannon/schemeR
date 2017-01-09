## A Lisp-like macro facility for R
## This is the non-hygienic version of macros, based on defmacro() as in
## Common Lisp, rather than on syntax objects and syntax-case.

#' Build a temporary symbol
#'
#' \code{gensym} is an analogue of Common Lisp's gensym. It builds symbols
#' which are guaranteed not to shadow or conflict with bindings already defined
#' in an environment, for use in writing macros.
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
