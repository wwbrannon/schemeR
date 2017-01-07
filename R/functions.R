### Various basic Scheme functions implemented for R. Unlike the versions in
### the "lisp" package, these work on both lists and vectors, and include more
### functions. Semantics are not always the same as in classic Scheme (e.g.,
### the empty list is not nil, because in R, is.null(pairlist()) but not
### is.null(list())).

## Aliases for things already available in R under other names
nil <- NULL
lambda <- `function`
define <- `<-`
map <- Map
display <- print
cons <- c
write <- cat
read <- readline
expt <- `**`
reverse <- rev
member <- `%in%`

is.nil <- is.null
is.procedure <- is.function
is.number <- is.numeric
is.boolean <- is.logical

## Numeric functions
is.even <- function(x) { return(x %% 2 == 0) }
is.odd  <- function(x) { return(x %% 2 == 1) }
is.zero <- function(x) { return(x == 0) }
is.positive <- function(x) { return(x > 0) }
is.negative <- function(x) { return(x < 0) }

## Assignment operators
## Unlike in MIT Scheme, there's no distinction between creating a binding
## and assigning a value, so define <=> set.
set <- define

## Functional operators
keep.matching <- Filter

delete.matching <-
function(f, x)
{
    return(Filter(Negate(f), x))
}

keep.matching.items <- keep.matching
delete.matching.items <- delete.matching
reduce <- Reduce

## Type-generic list- or vector-processing functions
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
    return(lst[-1])
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

#FIXME
cond <-
function(...)
{
    args <- list(...)

    for(val in args)
    {
        if(is.list(val))
        {
            test <- val[[1]]
            ret <- val[[2]]
        } else
        {
            test <- val[1]
            ret <- val[2]
        }

        if(test)
        {
            ret
        }
    }
}
