## Functional operators

#FIXME: compose and curry should set srcref attributes so that they
#pretty-print in a non-crazy way

#' Compose functions
#'
#' Function composition is a common operation in functional programming.
#' Given some number of functions, this function returns another function
#' which represents their composition: given three functions f, g, h,
#' compose(f, g, h) returns a function equivalent for all x to f(g(h(x))).
#'
#' The functions passed must all take a single argument.
#'
#' If some of the \code{...} arguments are not function objects, they are
#' resolved to function objects in the following way: character arguments
#' are looked up in the provided \code{where} environment; symbol arguments
#' are coerced to character and then looked up in the \code{where} environment;
#' other types of arguments, or symbol/character arguments which fail to
#' resolve to a function object, raise an error.
#'
#' @param ... Objects to compose. See 'Details'.
#' @param where An environment in which to resolve any character or
#' symbol arguments.
#'
#' @return A function representing the composition of the arguments.
#'
#' @examples
#' f <- function(x) x+1
#' g <- function(x) 3*x
#' h <- function(x) x^2
#'
#' compose(f, g, h)(2) #=> 13 == 3(2)^2 + 1
#' @export
compose <-
function(..., where=parent.frame())
{
    cl <- match.call()
    args <- list(...)

    resolv <- function(x) if(is.character(x))
        get(x, envir=where)
    else if(is.symbol(x))
        get(as.character(x), envir=where)
    else
        x
    args <- lapply(args, resolv)

    if(!all(vapply(args, is.function, logical(1))))
        stop("Arguments must be or resolve to functions")

    helper <- function(f, g) function(x) f(g(x))
    Reduce(helper, args)
}

#' @export
curry <-
function(f, ...)
{
    lst <- list(...)

    #Straight out of a CS textbook here, but uncurry() is a bit
    #more interesting
    func <- function(...) do.call(f, c(lst, list(...)))
    structure(func, .curried=TRUE) #we don't need to track how many times
}

#' @export
lazy.curry <-
function(f, ...)
{
    lst <- eval(substitute(alist(...)))

    func <- function(...) do.call(f, c(lst, list(...)))
    structure(func, .curried=TRUE)
}

#' @export
uncurry <-
function(f, ...)
{
    if(is.null(attr(f, ".curried")))
        stop("f must have been returned by curry() or lazy.curry()")

    #f is a closure: it closes over another function, the function that
    #was curried. So what we want to do is get that function's syntactic
    #name in the body of f, and resolve the reference to it in f's
    #enclosing environment. That's what the line below does.
    #
    #The magic-looking bit about body(f)[[2]] arises from the form used
    #in curry() - it's the first argument to do.call. (We could also have
    #just used the literal string "f", because it's fixed in the source code
    #of curry(), but that's confusing given the shadowed binding.)
    get(as.character(body(f)[[2]]), envir=environment(f), inherits=FALSE)
}

#' @export
lambda <-
function(...)
{
    args <- eval(substitute(alist(...)))

    if(length(args) <= 1)
        stop("Too few arguments to lambda")

    #This is easy; putting the formals together is harder
    body <- do.call(expression, args[2:length(args)])

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
