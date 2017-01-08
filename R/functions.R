### Various basic Scheme functions implemented for R. Unlike the versions in
### the "lisp" package, these work on both lists and vectors, and include more
### functions. Semantics are not always the same as in classic Scheme (e.g.,
### the empty list is not nil, because in R, is.null(pairlist()) but not
### is.null(list())).

## Aliases for things already available in R under other names
nil <- NULL
define <- `<-`
map <- Map
display <- print
cons <- c
append <- c #not exactly as in scheme, but close
expt <- `^`
reverse <- rev
member <- `%in%`
progn <- `{`
nth <- `[`
fromPkg <- `::`

is.nil <- is.null
is.procedure <- is.function
is.number <- is.numeric
is.boolean <- is.logical
quotient <- `%/%`
remainder <- `%%`
modulo <- remainder

## Other numeric functions
is.even <- function(x) { return(x %% 2 == 0) }
is.odd  <- function(x) { return(x %% 2 == 1) }
is.zero <- function(x) { return(x == 0) }
is.positive <- function(x) { return(x > 0) }
is.negative <- function(x) { return(x < 0) }

## Non-binary comparison operators
## eq, ge, le, gt, lt: =, >=, <=, >, <
eq <- function(...) { Reduce(`==`, list(...)) }
ge <- function(...) { Reduce(`>=`, list(...)) }
le <- function(...) { Reduce(`<=`, list(...)) }
gt <- function(...) { Reduce(`>`, list(...)) }
lt <- function(...) { Reduce(`<`, list(...)) }

## Assignment operators
## Unlike in Scheme, there's no distinction between creating a binding
## and assigning a value, so define <=> set.
set <- define

set.pos <-
function(nm, pos, val)
{
    target <- as.symbol(deparse(substitute(nm)))

    if(is.list(nm))
    {
        expr <- bquote(.(target)[[.(pos)]] <- .(val))
    } else
    {
        expr <- bquote(.(target)[.(pos)] <- .(val))
    }

    eval(expr, envir=parent.frame())
}

set.car <- function(nm, val) set.pos(nm=nm, val=val, pos=1)

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

## Functional operators
lambda <-
function(...)
{
    args <- eval(substitute(alist(...)))

    if(length(args) != 2)
        stop("Incorrect number of arguments to lambda")

    #This is easy; putting the formals together is harder
    body <- args[[2]]

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
            vals[[nm]] <- zeroLengthSymbol()
        } else
        {
            vals[[nm]] <- p[[2]] #don't eval
        }
    }

    fn <- eval(call("function", as.pairlist(vals), body), envir=parent.frame())
    environment(fn) <- parent.frame()

    fn
}

member.if <-
function(f, x, k=identity)
{
    for(i in seq_along(x))
    {
        if(f(k(x[[i]])))
            return(x[i:length(x)])
    }

    return(list())
}

zip <-
function(...)
{
    args <- list(function(...) { return(list(...)) })
    args <- c(args, list(...))
    args$SIMPLIFY <- FALSE

    return(do.call(mapply, args))
}

for.each <-
function(f, ...)
{
    params <- zip(list(...))

    for(p in params)
    {
        do.call(f, p)
    }

    return(invisible(NULL))
}

delete.matching <-
function(f, x)
{
    return(Filter(Negate(f), x))
}

keep.matching <- Filter
keep.matching.items <- keep.matching
delete.matching.items <- delete.matching
reduce <- Reduce

## Type-generic list- or vector-processing functions
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

## Lexical binding

#.(let,
#    .(i, 3),
#    .(foo, 5),
#  .(`==`, i, .(`-`, foo, 2)))
let <-
function(...)
{
    args <- eval(substitute(alist(...)))

    if(length(args) <= 1)
        stop("Too few arguments to let")

    for(a in args[1:(length(args) - 1)])
        if(length(a) != 2)
            stop("Invalid let binding")

    #The statement we want to evaluate
    body <- args[[length(args)]]
    args <- args[1:(length(args) - 1)]

    #We need to evaluate all the inits before setting any of the variables;
    #this is not necessarily the same as evaluating them all with parent
    #parent.frame() rather than env, because they may have side effects.
    vals <- lapply(args, function(x) eval(x[[2]], envir=parent.frame()))
    names(vals) <- vapply(args, function(x) as.character(x[[1]]), character(1))

    eval(body, envir=vals, enclos=parent.frame())
}

#.(let.star,
#    .(i, 3),
#    .(foo, 5),
#  .(`==`, i, .(`-`, foo, 2)))
let.star <-
function(...)
{
    args <- eval(substitute(alist(...)))

    if(length(args) <= 1)
        stop("Too few arguments to let.star")

    for(a in args[1:(length(args) - 1)])
        if(length(a) != 2)
            stop("Invalid let.star binding")

    #The statement we want to evaluate
    body <- args[[length(args)]]
    args <- args[1:(length(args) - 1)]

    #We need to "evaluate the bindings sequentially from left to right"
    env <- new.env(parent=parent.frame())
    for(arg in args)
    {
        nm <- as.character(arg[[1]])
        val <- eval(arg[[2]], envir=env)

        assign(nm, val, envir=env) #visible for subsequent evaluations
    }

    eval(body, envir=env)
}

#.(letrec,
#    .(i, 3),
#    .(foo, 5),
#  .(`==`, i, .(`-`, foo, 2)))

#.(letrec,
#    .(i, 3),
#    .(foo, .(lambda, .(n), .(`+`, n, 1))),
#  .(`==`, i, .(foo, 2)))
letrec <-
function(...)
{
    args <- eval(substitute(alist(...)))

    if(length(args) <= 1)
        stop("Too few arguments to letrec")

    for(a in args[1:(length(args) - 1)])
        if(length(a) != 2)
            stop("Invalid letrec binding")

    #We should evaluate the last statement passed with the caller's context
    #as environment - make this the body of the function built below
    body <- args[[length(args)]]
    args <- args[1:(length(args) - 1)]

    #The bindings become the formals of the generated function, because
    #function formals can be defined in a mutually recursive way
    vals <- lapply(args, function(x) x[[2]])
    names(vals) <- vapply(args, function(x) as.character(x[[1]]), character(1))

    fn <- eval(call("function", as.pairlist(vals), body), envir=parent.frame())
    environment(fn) <- parent.frame()

    return(fn())
}

## Flow-control operators
or <-
function(...)
{
    #Don't evaluate the args up front
    args <- eval(substitute(alist(...)))

    ret <- FALSE
    for(arg in args)
    {
        #only evaluate once in case there are side effects
        ret <- eval(arg)

        if(!identical(as.logical(ret), FALSE))
            break
    }

    return(ret)
}

and <-
function(...)
{
    #Don't evaluate the args up front
    args <- eval(substitute(alist(...)))

    ret <- TRUE
    for(arg in args)
    {
        #only evaluate once in case there are side effects
        ret <- eval(arg)

        if(identical(as.logical(ret), FALSE))
            break
    }

    return(ret)
}

#.(case, .(`+`, 1, 1),
#  .(3, .(print, "foo")),
#  .(2, .(print, "bar")))
case <-
function(...)
{
    args <- eval(substitute(alist(...)))
    if(length(args) <= 1)
        stop("Too few arguments to case")

    #The "key"
    val <- eval(args[[1]])
    args <- args[2:length(args)]

    for(i in seq_along(args))
    {
        clause <- args[[i]]

        if(length(clause) != 2)
            stop("Malformed case clause")

        for(obj in eval(clause[[1]]))
        {
            if(isTRUE(all.equal(val, obj)))
                return(eval(clause[[2]]))
        }
    }

    return(invisible(NULL))
}

#.(cond,
#  .(.(`==`, .(`+`, 1, 4), 4), .(print, "foo")),
#  .(.(`==`, .(`+`, 1, 3), 5), .(print, "bar")),
#  .(TRUE, .(print, "baz")))
cond <-
function(...)
{
    args <- eval(substitute(alist(...)))

    for(clause in args)
    {
        if(eval(clause[[1]]))
        {
            return(eval(clause[[2]]))
        }
    }

    return(invisible(NULL))
}
