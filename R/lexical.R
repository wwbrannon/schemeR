## Lexical binding

#FIXME: the bindings should all be under one list
#FIXME: better / more function-specific examples
#FIXME: various operators should take multiple expressions rather than just one

#'
#' @examples
#' schemeR::schemeR({
#'.(let,
#'    .(i, 3),
#'    .(foo, 5),
#'  .(`==`, i, .(`-`, foo, 2)))
#'  }, pkg=TRUE)
#' @rdname lexical
#' @export
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

#'
#' @examples
#' schemeR::schemeR({
#' .(let.star,
#'     .(i, 3),
#'     .(foo, 5),
#'   .(`==`, i, .(`-`, foo, 2)))
#'  }, pkg=TRUE)
#' @rdname lexical
#' @export
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

#'
#' @examples
#' schemeR::schemeR({
#' .(letrec,
#'     .(i, 3),
#'     .(foo, 5),
#'   .(`==`, i, .(`-`, foo, 2)))
#'  }, pkg=TRUE)
#'
#' schemeR::schemeR({
#' .(letrec,
#'     .(i, 3),
#'     .(foo, .(lambda, .(n), .(`+`, n, 1))),
#'   .(`==`, i, .(foo, 2)))
#'  }, pkg=TRUE)
#' @rdname lexical
#' @export
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
