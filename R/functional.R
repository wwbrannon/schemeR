## Functional operators

#FIXME: compose and curry should set srcref attributes so that they
#pretty-print in a non-crazy way
#FIXME: zip is producing warnings from mapply

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

#' Partial function application
#'
#' \code{curry} and related functions modify other functions by pre-setting
#' their arguments.
#'
#' For an introduction to the whole concept of function currying and why it's
#' useful, in more detail than we can give here, see the ever-helpful
#' \href{https://en.wikipedia.org/wiki/Currying}{Wikipedia}. Strictly speaking,
#' these functions do partial application rather than currying, but they can be
#' used to implement currying easily and the difference is small in practice.
#'
#' \code{curry} uses standard evaluation (i.e., does not implicitly quote its
#' arguments), while \code{lazy.curry} uses \code{substitute} to avoid
#' evaluating its arguments before using them in currying.
#'
#' \code{uncurry} takes a curried function - one resulting from a call to
#' \code{curry} or \code{lazy.curry} - and undoes the currying, returning the
#' original function without pre-set arguments.
#'
#' If currying is nested, one call to \code{uncurry} can be made for each call
#' to \code{curry} or \code{lazy.curry} - attempting to uncurry a function more
#' times than it's been curried will raise an error.
#'
#' @param f The function to curry or uncurry.
#' @param ... Arguments to be used in currying.
#'
#' @return For \code{curry} and \code{lazy.curry}, the curried function;
#' for \code{uncurry}, the uncurried function. Note that \code{uncurry}
#' will raise an error if its argument did not result from a call to
#' \code{curry} or \code{lazy.curry}.
#'
#' @section Note:
#' Currying is named after the mathematician
#' \href{http://en.wikipedia.org/wiki/Haskell_Curry}{Haskell Curry}.
#'
#' @examples
#' #Equivalent
#' f <- function(x) x + 6
#' g <- curry(`+`, 6)
#'
#' 4 + 2 == 6
#' curry(`+`, 2)(4) == 6
#'
#' paste0("foo", "bar") == "foobar"
#' uncurry(curry(paste0, "baz"))("foo", "bar") == "foobar"
#'
#' @rdname curry
#' @name curry
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

#' @rdname curry
#' @export
lazy.curry <-
function(f, ...)
{
    lst <- eval(substitute(alist(...)))

    func <- function(...) do.call(f, c(lst, list(...)))
    structure(func, .curried=TRUE)
}

#' @rdname curry
#' @export
uncurry <-
function(f)
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

#' Searching for tails of sequences
#'
#' \code{member.if} and \code{member.if.not} search "sequences" (by which we
#' mean lists, other vectors or pairlists) for the first element satisfying
#' some predicate function, and return the sub-sequence beginning with that
#' element.
#'
#' The sequence searched is actually \code{\link{map}(k, x)} rather than x,
#' which makes it easier to avoid defining short anonymous functions.
#'
#' @param f The filter predicate to use on the sequence x.
#' @param x The sequence to search for a satisfying element.
#' @param k The "key" function to pre-apply to elements of x. Defaults to
#' the identity function.
#'
#' @return The tail of the sequence \code{map(k, x)} beginning with the first
#' element that satisfies the predicate f, or NULL if no element did.
#'
#' @examples
#' f <- 20:40
#'
#' #The first element divisible by 3 and all afterward
#' member.if(is.zero, f, k=function(x) x %% 3) == 21:40
#'
#' #Trimming by the presence of a sentinel value
#' member.if.not(function(x) x < 30, f) == 30:40
#'
#' @rdname member-if
#' @name member-if
#' @export
member.if <-
function(f, x, k=identity)
{
    for(i in seq_along(x))
    {
        if(f(k(x[[i]])))
            return(x[i:length(x)])
    }

    return(NULL)
}

#' @rdname member-if
#' @export
member.if.not <-
function(f, x, k=identity)
{
    return(member.if(Negate(f), x=x, k=k))
}

#' Transposition of sequences
#'
#' \code{zip} takes lists, vectors or pairlists and creates new ones that
#' represent the transposition of the originals.
#'
#' "Transposition" here is meant in the following sense: given n sequences
#' of length m, return m sequences of length n. This sounds confusing but
#' is quite intuitive in practice - see the examples.
#'
#' For those familiar with Python, this is just like the built-in Python
#' \code{zip} function.
#'
#' @param ... Sequences to zip.
#'
#' @return The zipped sequences.
#'
#' @examples
#' f <- 1:5
#' g <- 5:10
#' h <- 11:15
#'
#' zip(f, g, h)
#'
#' @export
zip <-
function(...)
{
    args <- list(function(...) { return(list(...)) })
    args <- c(args, list(...))

    return(do.call(curry(mapply, SIMPLIFY=FALSE), args))
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
