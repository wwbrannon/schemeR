## Type-generic list- or vector-processing functions
#' @export
cons <- c

#' @export
nth <- `[`

#' @export
member <- `%in%`

#' @export
is.empty <- function(obj) length(obj) == 0

#' @export
make.list <-
function(n, expr)
{
    #As in base, but with a fixed different value for simplify
    sapply(integer(n), eval.parent(substitute(function(...) expr)),
           simplify=FALSE)
}

#' @export
car <-
function(obj)
{
    if(is.list(obj) || is.pairlist(obj))
    {
        return(obj[[1]])
    } else
    {
        return(obj[1])
    }
}

#' @export
cdr <-
function(lst)
{
    #Valid for both lists and vectors
    ret <- lst[-1]
    if(length(ret) != 0)
        return(ret)
    else
        return(NULL)
}

#' @export
last <-
function(obj)
{
    n <- length(obj)

    if(is.list(obj) || is.pairlist(obj))
    {
        return(obj[[n]])
    } else
    {
        return(obj[n])
    }
}

#' @export
first <- car

#' @export
caar <- function(lst) { return(car(car(lst))) }

#' @export
cadr <- function(lst) { return(car(cdr(lst))) }

#' @export
cdar <- function(lst) { return(cdr(car(lst))) }

#' @export
cddr <- function(lst) { return(cdr(cdr(lst))) }

#' @export
caaar <- function(lst) { return(car(car(car(lst)))) }

#' @export
caadr <- function(lst) { return(car(car(cdr(lst)))) }

#' @export
cadar <- function(lst) { return(car(cdr(car(lst)))) }

#' @export
caddr <- function(lst) { return(car(cdr(cdr(lst)))) }

#' @export
cdaar <- function(lst) { return(cdr(car(car(lst)))) }

#' @export
cdadr <- function(lst) { return(cdr(car(cdr(lst)))) }

#' @export
cddar <- function(lst) { return(cdr(cdr(car(lst)))) }

#' @export
cdddr <- function(lst) { return(cdr(cdr(cdr(lst)))) }

#' @export
caaaar <- function(lst) { return(car(car(car(car(lst))))) }

#' @export
caaadr <- function(lst) { return(car(car(car(cdr(lst))))) }

#' @export
caadar <- function(lst) { return(car(car(cdr(car(lst))))) }

#' @export
caaddr <- function(lst) { return(car(car(cdr(cdr(lst))))) }

#' @export
cadaar <- function(lst) { return(car(cdr(car(car(lst))))) }

#' @export
cadadr <- function(lst) { return(car(cdr(car(cdr(lst))))) }

#' @export
caddar <- function(lst) { return(car(cdr(cdr(car(lst))))) }

#' @export
cadddr <- function(lst) { return(car(cdr(cdr(cdr(lst))))) }

#' @export
cdaaar <- function(lst) { return(cdr(car(car(car(lst))))) }

#' @export
cdaadr <- function(lst) { return(cdr(car(car(cdr(lst))))) }

#' @export
cdadar <- function(lst) { return(cdr(car(cdr(car(lst))))) }

#' @export
cdaddr <- function(lst) { return(cdr(car(cdr(cdr(lst))))) }

#' @export
cddaar <- function(lst) { return(cdr(cdr(car(car(lst))))) }

#' @export
cddadr <- function(lst) { return(cdr(cdr(car(cdr(lst))))) }

#' @export
cdddar <- function(lst) { return(cdr(cdr(cdr(car(lst))))) }

#' @export
cddddr <- function(lst) { return(cdr(cdr(cdr(cdr(lst))))) }
