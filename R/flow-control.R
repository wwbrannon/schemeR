#' Flow-control operators
#'
#' These functions provide R versions of several Scheme flow-control operators.
#' \code{cond()} and \code{case()} are conditional forms, \code{do()} is an
#' iteration construct, and \code{or()} and \code{and()} allow for conditional
#' short-circuit evaluation. \code{when} and \code{unless} execute expressions
#' or not depending on the value of a test expression. See the vignettes for
#' full details and a more in-depth discussion of how to use these operators.
#'
#' The test expression for \code{do} is either one or two elements, and is
#' interpreted as follows: the first elment controls whether the loop
#' continues; the second element, if provided, is the return value of the
#' loop. If not provided, the first element's value is returned.
#'
#' The test expression for \code{when} and \code{unless} is a single
#' expression, and is evaluated non-lazily. \code{when} executes its
#' body expressions if the test expression is true; \code{unless}
#' executes them if the test expression is false or NULL.
#'
#' @param val The value dispatched by case and compared with the first elements
#' of the other clauses passed.
#' @param bindings A list of variable bindings for do. Each element is itself
#' two or three elements long; the first is a symbol, the second an initial
#' value, and the third an expression evaluated to update the variable on each
#' iteration. If no third element is provided, the variable is not updated.
#' @param test A test expression. See 'Details'.
#' @param ... The infix form of prefix arguments. Note that for \code{and()}
#' and \code{or()}, each element of ... is just an arbitrary R expression.
#'
#' @return
#' Return values are different for different operators.
#' \itemize{
#' \item{or returns the value of the first expression which when
#' coerced to logical is TRUE, or otherwise returns FALSE;}
#' \item{and returns the value of the first expression which when
#' coerced to logical is FALSE, or otherwise returns TRUE;}
#' \item{case returns the value of the expression associated with its key
#' expression's value, or if the key's value was not in any clause, returns
#' NULL;}
#' \item{cond returns the value of the last expression in the list of them
#' associated with the first clause whose test condition was logically TRUE,
#' or if no condition was TRUE, returns NULL;}
#' \item{do returns the value of the expression provided with its test
#' expression on the first iteration on which the test expression is logically
#' TRUE (if no second expression was provided, the value of the test expression
#' is returned).}
#' \item{when and unless return the value of the last body expression, if the
#' body expressions were evaluated, or NULL.}
#' }
#'
#' @examples
#' # the print("foo") clause will never be reached in either expression
#' schemeR::schemeR({
#' .(or, 1==2, TRUE, print("foo"))
#' .(and, 1==1, 3 > 4, print("foo"))
#'  }, pkg=TRUE)
#'
#' @rdname flow-control
#' @name flow-control
#' @export
or <-
function(...)
{
    #Don't evaluate the args up front
    args <- eval(substitute(alist(...)))

    ret <- FALSE
    for(arg in args)
    {
        #only evaluate once in case there are side effects
        e <- parent.frame()
        ret <- eval(arg, envir=e)

        if(!identical(as.logical(ret), FALSE))
            break
    }

    return(ret)
}

#' @rdname flow-control
#' @export
and <-
function(...)
{
    #Don't evaluate the args up front
    args <- eval(substitute(alist(...)))

    ret <- TRUE
    for(arg in args)
    {
        #only evaluate once in case there are side effects
        e <- parent.frame()
        ret <- eval(arg, envir=e)

        if(identical(as.logical(ret), FALSE))
            break
    }

    return(ret)
}

#' @rdname flow-control
#' @export
when <-
function(test, ...)
{
    args <- eval(substitute(alist(...)))
    body <- as.call(c(list(as.symbol("{")), args))

    if(!is.null(test) && test)
    {
        e <- parent.frame()
        eval(body, envir=e)
    }
    else
    {
        NULL
    }
}

#' @rdname flow-control
#' @export
unless <-
function(test, ...)
{
    args <- eval(substitute(alist(...)))
    body <- as.call(c(list(as.symbol("{")), args))

    if(is.null(test) || !test)
    {
        e <- parent.frame()
        eval(body, envir=e)
    }
    else
    {
        NULL
    }
}

#' @examples
#' schemeR::schemeR({
#' .(case, .(`+`, 1, 1),
#'   .(3, .(print, "foo")),
#'   .(2, .(print, "bar")))
#'  }, pkg=TRUE)
#' @rdname flow-control
#' @export
case <-
function(val, ...)
{
    args <- eval(substitute(alist(...)))

    if(length(args) == 0)
        stop("Too few arguments to case")

    for(i in seq_along(args))
    {
        clause <- args[[i]]

        if(length(clause) <= 1)
            stop("Malformed case clause")

        #Same hack as in the let constructs - see the comment block in let()
        if(clause[[1]] == as.symbol("list") ||
           clause[[1]] == as.symbol("pairlist"))
        {
            clause <- clause[-1]
        }

        #check both before and after possibly removing the (pair)list symbol
        if(length(clause) <= 1)
            stop("Malformed case clause")

        e <- parent.frame()

        lst <- eval(clause[[1]], envir=e)
        lst <- if(is.language(lst)) list(lst) else lst

        for(obj in lst)
        {
            if(isTRUE(all.equal(val, obj)))
            {
                #the implicit progn
                body <- c(list(as.symbol("{")),
                          as.list(clause[-1]))
                return(eval(as.call(body), envir=e))
            }
        }
    }

    return(invisible(NULL))
}

#' @examples
#' schemeR::schemeR({
#' .(cond,
#'   .(.(`==`, .(`+`, 1, 4), 4), .(print, "foo")),
#'   .(.(`==`, .(`+`, 1, 3), 5), .(print, "bar")),
#'   .(TRUE, .(print, "baz")))
#'  }, pkg=TRUE)
#' @rdname flow-control
#' @export
cond <-
function(...)
{
    args <- eval(substitute(alist(...)))

    for(clause in args)
    {
        if(length(clause) <= 1)
            stop("Malformed cond clause")

        #Same hack as in the let constructs - see the comment block in let()
        if(clause[[1]] == as.symbol("list") ||
           clause[[1]] == as.symbol("pairlist"))
        {
            clause <- clause[-1]
        }

        #check both before and after possibly removing the (pair)list symbol
        if(length(clause) <= 1)
            stop("Malformed cond clause")

        e <- parent.frame()
        if(eval(clause[[1]], envir=e))
        {
            body <- c(list(as.symbol("{")), as.list(clause[-1]))
            return(eval(as.call(body), envir=e))
        }
    }

    return(invisible(NULL))
}

#' @examples
#' schemeR::schemeR({
#' .(let, .(.(x, .(c, 1, 3, 5, 7, 9))),
#'   .(do, .(.(x, .(c, 1, 3, 5, 7, 9), .(cdr, x)),
#'           .(s, 0, .(`+`, s, .(car, x))),
#'           .(foo, 4)),
#'     .(.(is.nil, x), s),
#'     TRUE)) == 25
#'  }, pkg=TRUE)
#' @rdname flow-control
#' @export
do <-
function(bindings, test, ...)
{
    if(missing(test))
        stop("No test expression provided")

    bindings <- as.list(substitute(bindings))
    test <- substitute(test)
    args <- eval(substitute(alist(...)))

    #Same hack as in the let constructs - see the comment block in let()
    if(bindings[[1]] == as.symbol("list") ||
       bindings[[1]] == as.symbol("pairlist"))
    {
        bindings <- bindings[-1]
    }

    for(b in bindings)
    {
        if(length(b) %in% c(2,3))
            TRUE #pass
        else if(length(b) == 4 && b[[1]] == as.symbol("list"))
            TRUE #pass
        else if(length(b) == 4 && b[[1]] == as.symbol("pairlist"))
            TRUE #pass
        else
            stop("Invalid binding for do")
    }

    #The test clause
    if(length(test) == 0)
        stop("Invalid test clause for do")

    if(test[[1]] == as.symbol("list") ||
       test[[1]] == as.symbol("pairlist"))
    {
        test <- test[-1]
    }

    #check both before and after possibly removing the (pair)list symbol
    if(length(test) == 0)
        stop("Invalid test clause for do")

    #The test-is-false loop body, evaluated for effect
    if(length(args) > 0)
        cmd <- do.call(expression, args)
    else
        cmd <- quote(NULL)

    #The bindings and step expressions. If no step expression is given for
    #a variable "x", make "x" the step expression - it's a no-op that makes
    #this function's logic slightly simpler.
    lst <- list()
    steps <- list()

    for(binding in bindings)
    {
        if(binding[[1]] == as.symbol("list") ||
           binding[[1]] == as.symbol("pairlist"))
        {
            binding <- binding[-1]
        }

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
    e <- new.env(parent=parent.frame())
    for(i in seq_along(lst))
        assign(names(lst)[[i]], eval(lst[[i]], envir=parent.frame()), envir=e)

    #Begin iteration - each time around the loop, check test and decide
    #what to do. If needed, eval the step expressions in an environment
    #partially constructed from lst.
    while(TRUE)
    {
        ret <- eval(test[[1]], envir=e)
        if(ret) #end iteration
        {
            if(length(test) == 1) #no separate return expression
            {
                return(ret)
            } else # >= 2
            {
                body <- c(list(as.symbol("{")), as.list(test[-1]))
                return(eval(as.call(body), envir=e))
            }
        } else
        {
            #Evaluate this "for effect"
            eval(cmd, envir=e)

            #Use the step expressions to update the variables (in lst),
            #being sure to evaluate them in a context where the previous
            #values of the bindings are visible. The tmp environment and
            #the separate evaluation and rebinding steps make it so that
            #all step expressions are recomputed before any of the variables
            #are rebound, which is Scheme's behavior.
            tmp <- new.env(parent=emptyenv())

            for(i in seq_along(steps))
                assign(names(lst)[[i]], eval(steps[[i]], envir=e), envir=tmp)

            for(nm in ls(envir=tmp))
                assign(nm, get(nm, envir=tmp), envir=e)
        }
    }
}
