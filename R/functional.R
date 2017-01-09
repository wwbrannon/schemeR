## Functional operators

#' @export
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
            #This one takes some explanation: in building a function by
            #hand, the formal argument list is passed in to the "function"
            #constructor as a pairlist, whose tags are used as the argument
            #names. The values become the argument defaults. If you want an
            #argument with no default, which will raise an error if missing,
            #the value that has to be passed in is a zero-length symbol.
            #There's no obvious way to generate one of these: as.symbol("") and
            #related constructs all raise errors. But, as it turns out, alist()
            #returns them for arguments that have a tag but no value. The fact
            #that this little trick is possible saves us from having to write C
            #to generate the necessary zero-length name.
            vals[[nm]] <- alist(x=)$x
        } else
        {
            vals[[nm]] <- p[[2]] #don't eval
        }
    }

    fn <- eval(call("function", as.pairlist(vals), body), envir=parent.frame())
    environment(fn) <- parent.frame()

    fn
}

#' @export
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

#' @export
zip <-
function(...)
{
    args <- list(function(...) { return(list(...)) })
    args <- c(args, list(...))
    args$SIMPLIFY <- FALSE

    return(do.call(mapply, args))
}

#' @export
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

#' @export
delete.matching <-
function(f, x)
{
    return(Filter(Negate(f), x))
}

#' @export
map <- Map

#' @export
reverse <- rev

#' @export
reduce <- Reduce

#' @export
keep.matching <- Filter

#' @export
keep.matching.items <- keep.matching

#' @export
delete.matching.items <- delete.matching
