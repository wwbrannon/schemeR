## Flow-control operators

#' Flow-control operators
#'
#' These functions provide R versions of several Scheme flow-control operators.
#' \code{cond()} and \code{case()} are conditional forms, \code{do()} is an
#' iteration construct, and \code{or()} and \code{and()} allow for conditional
#' short-circuit evaluation. See the vignettes for full details and a more
#' in-depth discussion of how to use these operators.
#'
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
        ret <- eval(arg)

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
        ret <- eval(arg)

        if(identical(as.logical(ret), FALSE))
            break
    }

    return(ret)
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
        if(eval(clause[[1]]))
        {
            body <- do.call(expression, as.list(clause[2:length(clause)]))
            return(eval(body))
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
#'     TRUE)) #=> 25
#'  }, pkg=TRUE)
#' @rdname flow-control
#' @export
do <-
function(...)
{
    args <- eval(substitute(alist(...)))

    if(length(args) <= 1)
        stop("Too few arguments to do")

    #The test-is-false loop body
    if(length(args) >= 3)
    {
        cmd <- do.call(expression, args[3:length(args)])
    } else
    {
        cmd <- quote(NULL)
    }

    #The test clause
    tc <- args[[2]]
    if(length(tc) == 0)
        stop("Invalid test clause for do")

    test <- tc[[1]]
    test_expr <- ifelse(length(tc) >= 2,
                        do.call(expression, as.list(tc[2:length(tc)])),
                        NULL)

    args <- args[1:1]

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
