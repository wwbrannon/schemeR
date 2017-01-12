#' Lisp-friendly basic aliases
#'
#' These functions and other objects make things that are already available
#' in base R available under their traditional names in Scheme and other Lisp
#' dialects., and in some cases provide syntactic names for R operators that
#' would otherwise have to be backtick-quoted. See 'Details' for a full list.
#'
#' The aliases defined are:
#' \itemize{
#' \item{\code{display} is an alias for \code{print}}
#' \item{\code{progn} is an alias for \code{\{}}
#' \item{\code{begin} is an alias for \code{\{}}
#' \item{\code{fromPkg} is an alias for \code{::}}
#' \item{\code{is.nil} is an alias for \code{is.null}}
#' \item{\code{is.procedure} is an alias for \code{is.function}}
#' \item{\code{is.boolean} is an alias for \code{is.logical}}
#' \item{\code{nil} is an alias for \code{NULL}}
#' }
#'
#' See the respective help pages for the base names for more details.
#'
#' @param x An object.
#' @param ... Further arguments to pass to aliased base R functions.
#' @param pkg The package fromPkg should look in.
#' @param name The name of the object fromPkg should look for.
#'
#' @return Return values are as for the aliased base functions.
#'
#' @rdname basic-aliases
#' @name basic-aliases
#' @export
display <-
function(x, ...)
{
    return(do.call(curry(print, x), list(...)))
}

#' @rdname basic-aliases
#' @export
progn <-
function(...)
{
    args <- eval(substitute(alist(...)))
    lst <- c(list(as.symbol("{")), args)
    return(as.call(lst))
}

#' @rdname basic-aliases
#' @export
begin <- progn

#' @rdname basic-aliases
#' @export
fromPkg <-
function(pkg, name)
{
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))

    lst <- list(pkg=pkg, name=name)
    return(do.call(`::`, lst))
}

#' @rdname basic-aliases
#' @export
is.nil <-
function(x)
{
    return(is.null(x))
}

#' @rdname basic-aliases
#' @export
is.procedure <-
function(x)
{
    return(is.function(x))
}

#' @rdname basic-aliases
#' @export
is.boolean <-
function(x)
{
    return(is.logical(x))
}

#' @format NULL
#' @rdname basic-aliases
#' @export
nil <- NULL

#' Non-binary comparison operators
#'
#' These functions provide versions of R's built-in binary comparison operators
#' that work with arbitrarily many arguments. \code{ge} is the \code{>=}
#' operator, \code{le} is \code{<=}, \code{eq} is \code{==}, \code{lt} is
#' \code{<}, and \code{gt} is \code{>}.
#'
#' If more than 2 arguments are passed, the result is the logical conjunction
#' of the binary operator's return value for each pair of arguments (in order).
#' For example, le(1,2,3) is equivalent to 1 <= 2 <= 3 (mathematically
#' speaking; the second of those is not a valid R expression). In R,
#' le(1,2,3) is equivalent to 1 <= 2 && 2 <= 3, and so on for more arguments.
#'
#' @param ... Values to compare.
#'
#' @return The boolean result of the comparison.
#'
#' @examples
#' le(1,2,3) == TRUE
#'
#' ge(1,2,3) == FALSE
#'
#' eq(1,1,1) == TRUE
#'
#' eq(1,2,3) == FALSE
#'
#' @rdname nonbinary-comparison
#' @name nonbinary-comparison
#' @export
eq <-
function(...)
{
    lst <- rev(list(...))

    #Special cases
    if(length(lst) == 0)
        return(FALSE)

    if(length(lst) == 1)
    {
        if(is.null(lst[[1]]))
            return(FALSE)
        else
            return(TRUE)
    }

    #Sadly we can't use Reduce - the data types don't match
    ret <- TRUE
    for(i in seq_along(lst))
    {
        if(i == 1)
        {
            next
        }

        if(length(lst[[i]]) == 0 || length(lst[[i - 1]]) == 0)
        {
            ret <- FALSE
            break
        }

        if(!(lst[[i]] == lst[[i - 1]]))
        {
            ret <- FALSE
            break
        }
    }

    ret
}

#' @rdname nonbinary-comparison
#' @export
ge <-
function(...)
{
    lst <- rev(list(...))

    #Special cases
    if(length(lst) == 0)
        return(FALSE)

    if(length(lst) == 1)
    {
        if(is.null(lst[[1]]))
            return(FALSE)
        else
            return(TRUE)
    }

    #Sadly we can't use Reduce - the data types don't match
    ret <- TRUE
    for(i in seq_along(lst))
    {
        if(i == 1)
        {
            next
        }

        if(length(lst[[i]]) == 0 || length(lst[[i - 1]]) == 0)
        {
            ret <- FALSE
            break
        }

        if(!(lst[[i]] >= lst[[i - 1]]))
        {
            ret <- FALSE
            break
        }
    }

    ret
}

#' @rdname nonbinary-comparison
#' @export
le <-
function(...)
{
    lst <- rev(list(...))

    #Special cases
    if(length(lst) == 0)
        return(FALSE)

    if(length(lst) == 1)
    {
        if(is.null(lst[[1]]))
            return(FALSE)
        else
            return(TRUE)
    }

    #Sadly we can't use Reduce - the data types don't match
    ret <- TRUE
    for(i in seq_along(lst))
    {
        if(i == 1)
        {
            next
        }

        if(length(lst[[i]]) == 0 || length(lst[[i - 1]]) == 0)
        {
            ret <- FALSE
            break
        }

        if(!(lst[[i]] <= lst[[i - 1]]))
        {
            ret <- FALSE
            break
        }
    }

    ret
}

#' @rdname nonbinary-comparison
#' @export
gt <-
function(...)
{
    lst <- rev(list(...))

    #Special cases
    if(length(lst) == 0)
        return(FALSE)

    if(length(lst) == 1)
    {
        if(is.null(lst[[1]]))
            return(FALSE)
        else
            return(TRUE)
    }

    #Sadly we can't use Reduce - the data types don't match
    ret <- TRUE
    for(i in seq_along(lst))
    {
        if(i == 1)
        {
            next
        }

        if(length(lst[[i]]) == 0 || length(lst[[i - 1]]) == 0)
        {
            ret <- FALSE
            break
        }

        if(!(lst[[i]] > lst[[i - 1]]))
        {
            ret <- FALSE
            break
        }
    }

    ret
}

#' @rdname nonbinary-comparison
#' @export
lt <-
function(...)
{
    lst <- rev(list(...))

    #Special cases
    if(length(lst) == 0)
        return(FALSE)

    if(length(lst) == 1)
    {
        if(is.null(lst[[1]]))
            return(FALSE)
        else
            return(TRUE)
    }

    #Sadly we can't use Reduce - the data types don't match
    ret <- TRUE
    for(i in seq_along(lst))
    {
        if(i == 1)
        {
            next
        }

        if(length(lst[[i]]) == 0 || length(lst[[i - 1]]) == 0)
        {
            ret <- FALSE
            break
        }

        if(!(lst[[i]] < lst[[i - 1]]))
        {
            ret <- FALSE
            break
        }
    }

    ret
}
