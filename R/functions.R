### Various basic Scheme functions implemented for R. Unlike the versions in
### the "lisp" package, these work on both lists and vectors, and include more
### functions. Semantics are not always the same as in classic Scheme (e.g.,
### the empty list is not nil, because in R, is.null(pairlist()) but not
### is.null(list())).

## Some basic / misc renamings of things R already has
nil <- NULL
define <- `<-`
display <- print
progn <- `{`
fromPkg <- `::`

is.nil <- is.null
is.procedure <- is.function
is.boolean <- is.logical

## Numeric functions
expt <- `^`
is.number <- is.numeric
quotient <- `%/%`
remainder <- `%%`
modulo <- remainder

sum <- function(...) { Reduce(`+`, list(...)) }
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

map <- Map
reverse <- rev
reduce <- Reduce
keep.matching <- Filter
keep.matching.items <- keep.matching
delete.matching.items <- delete.matching

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

#.(let, .(x, .(c, 1, 3, 5, 7, 9)),
#  .(do, .(.(x, .(c, 1, 3, 5, 7, 9), .(cdr, x)),
#          .(s, 0, .(`+`, s, .(car, x))),
#          .(foo, 4)),
#    .(.(is.nil, x), s),
#    T)) #=> 25
do <-
function(...)
{
    args <- eval(substitute(alist(...)))

    if(!(length(args) %in% c(2,3)))
        stop("Incorrect number of arguments to do")

    #The test-is-false loop body
    if(length(args) == 3)
    {
        cmd <- args[[length(args)]]
        args <- args[1:(length(args) - 1)]
    } else
    {
        cmd <- quote(NULL)
    }

    #The test clause
    tc <- args[[length(args)]]
    if(!(length(tc) %in% c(1,2)))
        stop("Invalid test clause for do")

    args <- args[1:(length(args) - 1)]

    test <- tc[[1]]
    test_expr <- ifelse(length(tc) == 2, tc[[2]], NULL)

    #The bindings and step expressions. If no step expression is given for
    #a variable "x", make "x" the step expression - it's a no-op that makes
    #this function's logic slightly simpler.
    lst <- list()
    steps <- list()

    args <- rapply(as.list(args), as.list, how="replace")[[1]]
    for(binding in args)
    {
        if(!(length(binding) %in% c(2,3)))
            stop("Invalid binding for do")

        nm <- as.character(binding[[1]])
        init <- binding[[2]]

        if(length(binding) == 3)
        {
            step <- binding[[3]]
        } else
        {
            step <- as.symbol(nm)
        }

        lst[[nm]] <- init
        steps[[nm]] <- step
    }

    #Evaluate the init expressions, but not (yet) the step expressions
    nms <- names(lst)
    lst <- lapply(lst, eval)
    names(lst) <- nms

    #Begin iteration - each time around the loop, check test and decide
    #what to do. If needed, eval the step expressions in an environment
    #partially constructed from lst.
    while(TRUE)
    {
        #FIXME - several places in this file assume that parent.frame()
        #descends from baseenv()
        ret <- eval(test, envir=lst, enclos=parent.frame())
        if(ret)
        {
            if(!is.null(test_expr))
            {
                return(eval(test_expr, envir=lst, enclos=parent.frame()))
            } else
            {
                return(ret)
            }
        } else
        {
            #Evaluate this "for effect"
            eval(cmd, envir=lst, enclos=parent.frame())

            #Update the step expressions, being sure to evaluate them
            #in a context where the previous values of the bindings are
            #visible
            nms <- names(lst)
            fn <- function(x) eval(x, envir=lst, enclos=parent.frame())
            lst <- lapply(steps, fn)
            names(lst) <- nms
        }
    }
}
