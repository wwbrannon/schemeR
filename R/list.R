## Type-generic list- or vector-processing functions

#' List utilities
#'
#' These functions provide small list- or vector-processing utilities, and in
#' some cases are just aliases for functions in base R. In such cases, the
#' point of the alias is to make the function available under the standard
#' Scheme name.
#'
#' \code{cons} is an alias for base R's \code{\link{c}}, \code{nth} is an alias
#' for \code{\link{[[}}, \code{member} is an alias for \code{\link{\%in\%}},
#' \code{reverse} is an alias for \code{\link{rev}} and \code{is.empty} returns
#' TRUE if and only if its argument is of length 0, returning FALSE otherwise.
#'
#' \code{make.list} returns a list constructed by replicating its expr argument
#' n times. It is equivalent to \code{link{replicate}} with
#' \code{simplify=FALSE}.
#'
#' @param ... Arguments that \code{cons} should pass to \code{c()}.
#' @param x As in \code{\link{\%in\%}} or \code{\link{rev}}.
#' @param table As in \code{\link{\%in\%}}.
#' @param obj An object that is.empty should check the length of.
#' @param n The number of times \code{make.list} should replicate its expr
#' argument in building a list.
#' @param expr An object which \code{make.list} should replicate in building
#' a list.
#'
#' @return
#' \code{make.list} returns the n-length list of replicated expr's. is.empty returns
#' TRUE if its argument is 0-length and FALSE otherwise.
#'
#' @rdname list-utilities
#' @name list-utilities
#' @export
cons <-
function(...)
{
    do.call(c, list(...))
}

#' @rdname list-utilities
#' @export
reverse <- rev

#' @rdname list-utilities
#' @export
nth <-
function(obj, n)
{
    obj[[n]]
}

#' @rdname list-utilities
#' @export
member <- `%in%`

#' @rdname list-utilities
#' @export
is.empty <- function(obj) length(obj) == 0

#' @rdname list-utilities
#' @export
make.list <-
function(n, expr)
{
    #As in base, but with a fixed different value for simplify
    sapply(integer(n), eval.parent(substitute(function(...) expr)),
           simplify=FALSE)
}

#' List/vector access functions
#'
#' These functions are accessors for various elements and subsequences of lists
#' and vectors. They are written in a type-generic way and will work for any
#' broadly defined sequence type: vectors, lists (aka "generic vectors") and
#' pairlists.
#'
#' Using "sequence" to mean list, other vector or pairlist, the basic functions
#' here are
#' \itemize{
#' \item{car, which returns the first element of a sequence (car(x) is
#' equivalent to x[[1]] for (pair)lists and to x[1] for vectors)}
#' \item{cdr, which returns the sequence consisting of every element but
#' the first, or NULL for a sequence of length 0 or length 1}
#' \item{last, which returns the last element of a sequence}
#' \item{first, which is an alias for car}
#' }
#'
#' There are also a large number of functions of the form cXXXXr, where there
#' are two, three or four "a"'s or "d"'s between the c and r. These functions
#' are compositions of car and cdr: to give one example, \code{caadr(x)} is
#' equivalent to car(car(cdr(x))). All such functions with up to four letters
#' between the c and the r are pre-defined here.
#'
#' @param x The object whose elements or subsequences to access.
#'
#' @return A particular element or subsequence.
#' @rdname list-access
#' @name list-access
#' @export
car <-
function(x)
{
    if(length(x) == 0)
        NULL
    else
        x[[1]]
}

#' @rdname list-access
#' @export
cdr <-
function(x)
{
    ret <- x[-1]

    if(length(ret) == 0)
        NULL
    else if(is.pairlist(x))
        as.pairlist(ret)
    else
        ret
}

#' @rdname list-access
#' @export
first <- car

#' @rdname list-access
#' @export
last <- compose(car, reverse)

#' @rdname list-access
#' @export
caar <- compose(car, car)

#' @rdname list-access
#' @export
cadr <- compose(car, cdr)

#' @rdname list-access
#' @export
cdar <- compose(cdr, car)

#' @rdname list-access
#' @export
cddr <- compose(cdr, cdr)

#' @rdname list-access
#' @export
caaar <- compose(car, car, car)

#' @rdname list-access
#' @export
caadr <- compose(car, car, cdr)

#' @rdname list-access
#' @export
cadar <- compose(car, cdr, car)

#' @rdname list-access
#' @export
caddr <- compose(car, cdr, cdr)

#' @rdname list-access
#' @export
cdaar <- compose(cdr, car, car)

#' @rdname list-access
#' @export
cdadr <- compose(cdr, car, cdr)

#' @rdname list-access
#' @export
cddar <- compose(cdr, cdr, car)

#' @rdname list-access
#' @export
cdddr <- compose(cdr, cdr, cdr)

#' @rdname list-access
#' @export
caaaar <- compose(car, car, car, car)

#' @rdname list-access
#' @export
caaadr <- compose(car, car, car, cdr)

#' @rdname list-access
#' @export
caadar <- compose(car, car, cdr, car)

#' @rdname list-access
#' @export
caaddr <- compose(car, car, cdr, cdr)

#' @rdname list-access
#' @export
cadaar <- compose(car, cdr, car, car)

#' @rdname list-access
#' @export
cadadr <- compose(car, cdr, car, cdr)

#' @rdname list-access
#' @export
caddar <- compose(car, cdr, cdr, car)

#' @rdname list-access
#' @export
cadddr <- compose(car, cdr, cdr, cdr)

#' @rdname list-access
#' @export
cdaaar <- compose(cdr, car, car, car)

#' @rdname list-access
#' @export
cdaadr <- compose(cdr, car, car, cdr)

#' @rdname list-access
#' @export
cdadar <- compose(cdr, car, cdr, car)

#' @rdname list-access
#' @export
cdaddr <- compose(cdr, car, cdr, cdr)

#' @rdname list-access
#' @export
cddaar <- compose(cdr, cdr, car, car)

#' @rdname list-access
#' @export
cddadr <- compose(cdr, cdr, car, cdr)

#' @rdname list-access
#' @export
cdddar <- compose(cdr, cdr, cdr, car)

#' @rdname list-access
#' @export
cddddr <- compose(cdr, cdr, cdr, cdr)
