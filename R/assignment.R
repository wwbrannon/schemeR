#' Assignment operators
#'
#' These functions provide three in-place modification operators for sequences
#' (where "sequence" means vector, list or pairlist), and two
#' Lisp/Scheme-friendly aliases for R's built-in assignment operator \code{<-}.
#'
#' Three in-place modification operators are provided: \code{set.pos},
#' \code{set.car} and \code{set.cdr}, analogous to functions frequently found
#' in Lisp dialects. \code{set.pos} modifies the element at a particular
#' positional index in a sequence, and \code{set.car} is a special case of
#' \code{set.pos} for pos = 1. \code{set.cdr} modifies the subsequence that
#' excludes the first element.
#'
#' These operators do "in-place" modification in the sense that they modify the
#' binding of the symbol that is the first argument without the need for an
#' explicit assignment statement.
#'
#' The \code{define} and \code{set} functions are aliases for the built-in
#' \code{<-}, and are traditional names for variable creation and assignment
#' operators in Scheme. Unlike in Scheme, there's no distinction in R between
#' creating a binding and assigning a value (equivalently, it's not possible
#' to create an unassigned variable), so define is equivalent to set here.
#'
#' @param nm The target symbol, whose value (a sequence, in the sense of a
#' list, vector or pairlist) should be modified as appropriate and the symbol
#' rebound.
#' @param pos The index (into the target sequence) that set.pos should change.
#' @param val The new value to assign to the target component of the target
#' object.
#'
#' @return The new value.
#'
#' @examples
#' f <- 1:10
#' set.car(f, 4)
#' f == c(4, 2:10)
#'
#' f <- 1:10
#' set.cdr(f, 1:5)
#' f == c(1, 1:5)
#'
#' f <- 1:10
#' set.pos(f, 3, 10)
#' f == c(1, 2, 10, 4:10)
#'
#' @rdname assignment
#' @name assignment
#' @export
define <- `<-`

#' @rdname assignment
#' @export
set <- define

#' @rdname assignment
#' @export
set.pos <-
function(nm, pos, val)
{
    target <- as.symbol(deparse(substitute(nm)))

    expr <- bquote(.(target)[[.(pos)]] <- .(val))
    eval(expr, envir=parent.frame())
}

#' @rdname assignment
#' @export
set.car <-
function(nm, val)
{
    target <- as.symbol(deparse(substitute(nm)))

    expr <- bquote(set.pos(nm=.(target), val=.(val), pos=1))
    eval(expr, envir=parent.frame())
}

#' @rdname assignment
#' @export
set.cdr <-
function(nm, val)
{
    target <- as.symbol(deparse(substitute(nm)))

    if(is.list(nm))
    {
        expr <- bquote(.(target) <- c(.(target)[1], .(val)))
    } else
    {
        expr <- bquote(.(target)[2:length( .(target) )] <- .(val))
    }

    eval(expr, envir=parent.frame())
}
