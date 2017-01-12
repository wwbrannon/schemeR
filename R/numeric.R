#' Numeric functions
#'
#' These functions provide Lisp/Scheme-friendly aliases for and thin wrappers
#' around some numeric infix operators from base R.
#'
#' The expt, quotient, remainder and modulo functions are aliases for the
#' following base operators:
#' \itemize{
#' \item{\code{expt} is \code{^}}
#' \item{\code{quotient} is \code{\%/\%}}
#' \item{\code{remainder} is \code{\%\%}}
#' \item{\code{modulo} is also \code{\%\%}}
#' }
#'
#' The is.number function is an alias for the is.numeric function from base;
#' the other is.* functions are simple predicates that do what their names
#' suggest.
#'
#' @param e1 For expt, quotient, remainder and modulo, the left-hand argument
#' to the underlying infix operator.
#' @param e2 For expt, quotient, remainder and modulo, the right-hand argument
#' to the underlying infix operator.
#' @param x An R object on which to check a predicate.
#'
#' @return For the is.* functions, TRUE or FALSE; for expt, quotient, remainder
#' and modulo, the result of the operation.
#'
#' @examples
#' expt(4, 5) == 4^5 #=> 1024
#'
#' quotient(10, 3) == 10 %/% 3 #=> 3
#'
#' remainder(10, 3) == 10 %% 3 #=> 1
#'
#' modulo(10, 3) == 10 %% 3 #=> 1
#'
#' is.number(3) && !is.number("foo")
#'
#' is.even(22) && !is.even(23)
#'
#' is.odd(23) && !is.odd(22)
#'
#' is.zero(0) && !is.zero(1)
#'
#' is.positive(234) && !is.positive(-34)
#'
#' is.negative(-34) && !is.negative(234)
#'
#' @rdname numeric
#' @name numeric
#' @export
expt <-
function(e1, e2)
{
    if(is.nil(e1) || is.nil(e2))
        nil
    else
        e1 ^ e2
}

#' @rdname numeric
#' @export
quotient <-
function(e1, e2)
{
    if(is.nil(e1) || is.nil(e2))
        nil
    else
        e1 %/% e2
}

#' @rdname numeric
#' @export
remainder <-
function(e1, e2)
{
    if(is.nil(e1) || is.nil(e2))
        nil
    else
        e1 %% e2
}

#' @rdname numeric
#' @export
modulo <- remainder

#' @rdname numeric
#' @export
is.number <-
function(x)
{
    is.numeric(x)
}

#' @rdname numeric
#' @export
is.even <-
function(x)
{
    if(length(x) == 0)
        FALSE
    else if(!is.numeric(x))
        stop("x must be numeric")
    else
        x %% 2 == 0
}

#' @rdname numeric
#' @export
is.odd <-
function(x)
{
    if(length(x) == 0)
        FALSE
    else if(!is.numeric(x))
        stop("x must be numeric")
    else
        x %% 2 == 1
}

#' @rdname numeric
#' @export
is.zero <-
function(x)
{
    if(length(x) == 0)
        FALSE
    else if(!is.numeric(x))
        stop("x must be numeric")
    else
        x == 0
}

#' @rdname numeric
#' @export
is.positive <-
function(x)
{
    if(length(x) == 0)
        FALSE
    else if(!is.numeric(x))
        stop("x must be numeric")
    else
        x > 0
}

#' @rdname numeric
#' @export
is.negative <-
function(x)
{
    if(length(x) == 0)
        FALSE
    else if(!is.numeric(x))
        stop("x must be numeric")
    else
        x < 0
}
