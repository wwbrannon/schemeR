## Type-generic list- or vector-processing functions
cons <- c
append <- c #not exactly as in scheme, but close
nth <- `[`
member <- `%in%`

is.empty <- function(obj) length(obj) == 0

make.list <-
function(n, expr)
{
    #As in base, but with a fixed different value for simplify
    sapply(integer(n), eval.parent(substitute(function(...) expr)),
           simplify=FALSE)
}

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

first <- car
caar <- function(lst) { return(car(car(lst))) }
cadr <- function(lst) { return(car(cdr(lst))) }
cdar <- function(lst) { return(cdr(car(lst))) }
cddr <- function(lst) { return(cdr(cdr(lst))) }
caaar <- function(lst) { return(car(car(car(lst)))) }
caadr <- function(lst) { return(car(car(cdr(lst)))) }
cadar <- function(lst) { return(car(cdr(car(lst)))) }
caddr <- function(lst) { return(car(cdr(cdr(lst)))) }
cdaar <- function(lst) { return(cdr(car(car(lst)))) }
cdadr <- function(lst) { return(cdr(car(cdr(lst)))) }
cddar <- function(lst) { return(cdr(cdr(car(lst)))) }
cdddr <- function(lst) { return(cdr(cdr(cdr(lst)))) }
caaaar <- function(lst) { return(car(car(car(car(lst))))) }
caaadr <- function(lst) { return(car(car(car(cdr(lst))))) }
caadar <- function(lst) { return(car(car(cdr(car(lst))))) }
caaddr <- function(lst) { return(car(car(cdr(cdr(lst))))) }
cadaar <- function(lst) { return(car(cdr(car(car(lst))))) }
cadadr <- function(lst) { return(car(cdr(car(cdr(lst))))) }
caddar <- function(lst) { return(car(cdr(cdr(car(lst))))) }
cadddr <- function(lst) { return(car(cdr(cdr(cdr(lst))))) }
cdaaar <- function(lst) { return(cdr(car(car(car(lst))))) }
cdaadr <- function(lst) { return(cdr(car(car(cdr(lst))))) }
cdadar <- function(lst) { return(cdr(car(cdr(car(lst))))) }
cdaddr <- function(lst) { return(cdr(car(cdr(cdr(lst))))) }
cddaar <- function(lst) { return(cdr(cdr(car(car(lst))))) }
cddadr <- function(lst) { return(cdr(cdr(car(cdr(lst))))) }
cdddar <- function(lst) { return(cdr(cdr(cdr(car(lst))))) }
cddddr <- function(lst) { return(cdr(cdr(cdr(cdr(lst))))) }
