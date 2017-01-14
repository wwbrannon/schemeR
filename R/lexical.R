#FIXME: better / more function-specific examples

#' Lexical binding constructs
#'
#' These functions provide the let, let* and letrec block-scope operators
#' from Scheme. All three of them create temporary variables with specified
#' values, successively evaluate provided body expressions, and return the
#' value of the last expression.
#'
#' All three forms are syntactically identical:
#'
#' \code{
#' .(LET_OP, .(.(variable, init) ...), expr, expr ...)
#' }
#'
#' where LET_OP is \code{let}, \code{let.star} or \code{letrec}.
#'
#' They differ in the scope in which the temporary bindings (of names to init
#' expressions' values) exist:
#' \itemize{
#' \item{let computes the init expressions before creating any bindings.}
#' \item{let.star creates the bindings, and in the process evaluates the init
#' expressions, sequentially left to right, so that earlier bindings are in
#' scope for evaluating later init expressions.}
#' \item{letrec evaluates all init expressions simultaneously, with all
#' bindings in scope during evaluation. (The init expressions can therefore
#' be mutually recursive.)}
#' }
#'
#' @section Note:
#' R, unlike Scheme, does not have a notion of variable definition separate
#' from assignment. So the init expressions are mandatory here, whereas in
#' Scheme it is possible to create uninitialized variables in the scope of the
#' body block.
#'
#' The let, let.star and letrec functions take arguments in a way that seems
#' natural in \code{\link{prefix}} notation, but makes for odd-looking calls
#' in the usual infix form. Calling them "directly" - i.e., other than through
#' prefix code and SchemeR() or infix() - is not recommended.
#'
#' @param bindings A list whose elements are themselves two element lists: the
#' first element of each is a symbol, the second the value the symbol should be
#' bound to in the environment for evaluating the body statements.
#' @param ... Body statements.
#'
#' @return The result of evaluating the final body expression.
#'
#' @examples
#' schemeR::schemeR({
#'.(let, .(.(i, 3),
#'         .(foo, 5)),
#'    .(`==`, i, .(`-`, foo, 2)))
#'  }, pkg=TRUE) == TRUE
#' @rdname lexical
#' @name lexical
#' @export
let <-
function(bindings, ...)
{
    bindings <- as.list(substitute(bindings))
    args <- eval(substitute(alist(...)))

    if(length(args) == 0)
        stop("Too few arguments to let")

    #This is a hack to support calls to let which use in-line bindings lists
    #constructed through explicit calls to list(), rather than using do.call
    #to build the call from a previously defined list with quoted value
    #expressions. In this case, we won't see a list of two elements, we'll
    #see a parse tree and can't just evaluate the top level of it: we have
    #to handle a first element in the bindings which is the symbol "list", and
    #subsequent elements which are themselves three elements long:
    #list(var, exp). Same in let.star and letrec below.
    if(bindings[[1]] == as.symbol("list") ||
       bindings[[1]] == as.symbol("pairlist"))
    {
        bindings <- bindings[2:length(bindings)]
    }

    for(b in bindings)
    {
        if(length(b) == 2)
            TRUE #pass
        else if(length(b) == 3 && b[[1]] == as.symbol("list"))
            TRUE #pass
        else if(length(b) == 3 && b[[1]] == as.symbol("pairlist"))
            TRUE #pass
        else
            stop("Invalid let binding")
    }

    body <- as.call(c(list(as.symbol("{")), args)) #the implicit progn

    #We need to evaluate all the inits before setting any of the variables;
    #this is not necessarily the same as evaluating them all with parent
    #parent.frame() rather than env, because they may have side effects.
    vals <- lapply(bindings, function(x) eval(x[[length(x)]], envir=parent.frame()))
    names(vals) <- vapply(bindings, function(x) as.character(x[[length(x) - 1]]),
                          character(1))

    eval(body, envir=vals, enclos=parent.frame())
}

#' @examples
#' schemeR::schemeR({
#' .(let.star, .(.(i, 3), .(foo, 5)),
#'       .(`==`, i,
#'               .(`-`, foo, 2)))
#'  }, pkg=TRUE) == TRUE
#' @rdname lexical
#' @export
let.star <-
function(bindings, ...)
{
    bindings <- as.list(substitute(bindings))
    args <- eval(substitute(alist(...)))

    if(length(args) == 0)
        stop("Too few arguments to let")

    if(bindings[[1]] == as.symbol("list") ||
       bindings[[1]] == as.symbol("pairlist"))
    {
        bindings <- bindings[2:length(bindings)]
    }

    for(b in bindings)
    {
        if(length(b) == 2)
            TRUE #pass
        else if(length(b) == 3 && b[[1]] == as.symbol("list"))
            TRUE #pass
        else if(length(b) == 3 && b[[1]] == as.symbol("pairlist"))
            TRUE #pass
        else
            stop("Invalid let binding")
    }

    body <- do.call(expression, args) #the implicit progn

    #We need to "evaluate the bindings sequentially from left to right"
    env <- new.env(parent=parent.frame())
    for(bind in bindings)
    {
        nm <- as.character(bind[[length(bind) - 1]])
        val <- eval(bind[[length(bind)]], envir=env)

        assign(nm, val, envir=env) #visible for subsequent evaluations
    }

    eval(body, envir=env, enclos=parent.frame())
}

#' @examples
#' schemeR::schemeR({
#' .(letrec, .(.(i, 3), .(foo, 5)),
#'      .(`==`, i,
#'              .(`-`, foo, 2)))
#'  }, pkg=TRUE) == TRUE
#'
#' schemeR::schemeR({
#' .(letrec, .(.(i, 3),
#'             .(foo, .(lambda, .(n), .(`+`, n, 1)))),
#'      .(`==`, i, .(foo, 2)))
#'  }, pkg=TRUE) == TRUE
#' @rdname lexical
#' @export
letrec <-
function(bindings, ...)
{
    bindings <- as.list(substitute(bindings))
    args <- eval(substitute(alist(...)))

    if(length(args) == 0)
        stop("Too few arguments to let")

    if(bindings[[1]] == as.symbol("list") ||
       bindings[[1]] == as.symbol("pairlist"))
    {
        bindings <- bindings[2:length(bindings)]
    }

    for(b in bindings)
    {
        if(length(b) == 2)
            TRUE #pass
        else if(length(b) == 3 && b[[1]] == as.symbol("list"))
            TRUE #pass
        else if(length(b) == 3 && b[[1]] == as.symbol("pairlist"))
            TRUE #pass
        else
            stop("Invalid let binding")
    }

    body <- as.call(c(list(as.symbol("{")), args)) #the implicit progn

    #The bindings become the formals of the generated function, because
    #function formals can be defined in a mutually recursive way
    vals <- lapply(bindings, function(x) x[[length(x)]])
    names(vals) <- vapply(bindings, function(x) as.character(x[[length(x) - 1]]),
                          character(1))

    fn <- eval(call("function", as.pairlist(vals), body), envir=parent.frame())
    environment(fn) <- parent.frame()

    #Unlike in lambda, call it: fn only exists as a block scoping
    #mechanism, we care about the return value
    return(fn())
}
