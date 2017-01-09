## Flow-control operators
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

#.(case, .(`+`, 1, 1),
#  .(3, .(print, "foo")),
#  .(2, .(print, "bar")))
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

#.(cond,
#  .(.(`==`, .(`+`, 1, 4), 4), .(print, "foo")),
#  .(.(`==`, .(`+`, 1, 3), 5), .(print, "bar")),
#  .(TRUE, .(print, "baz")))
#' @export
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
#' @export
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
