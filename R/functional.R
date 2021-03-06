# Resolve function objects or names
#
# This function takes a list of objects, which may be functions, symbols or
# character strings, and resolves them to function objects. Functions are
# returned as-is; strings are looked up in the provided environment; symbols
# are coerced to character and looked up in the provided environment. If after
# this lookup process, any of the elements of fns are not functions, an error
# is raised.
#
# @param fns A list of functions, symbols or length-1 character vectors.
# @param envir The environment in which to look up non-function objects.
#
# @return The list of resolved function objects.
resolv <-
function(fns, envir)
{
    fns <-
    lapply(fns, function(x)
    {
        if(is.character(x) || is.symbol(x))
            get(as.character(x), envir=envir)
        else
            x
    })

    if(!all(vapply(fns, is.function, logical(1))))
        stop("Arguments must be or resolve to functions")

    fns
}

#' Construct an anonymous function in infix code
#'
#' This function provides the Lisp/Scheme lambda form for defining anonymous
#' functions. Its behavior is very similar to R's "function" keyword, but it
#' interprets its arguments in a way that allows it to be used naturally in
#' prefix code. It is not intended to be called the usual way from infix R
#' code.
#'
#' Because R is a Scheme dialect, this lambda is directly evaluable and
#' evaluates to a function object. Common Lisp's sharp-quote operator is not
#' necessary and not provided.
#'
#' @param params A parameter list, which when coerced to pairlist is acceptable
#' to the "function" constructor function.
#' @param ... Body statements.
#'
#' @return The constructed function.
#'
#' @examples
#' schemeR::schemeR({
#' .(map, .(lambda, .(n), .(paste0, n, "\n")),
#'        .(list, "a", "b"))
#' }, pkg=TRUE)
#'
#' schemeR::schemeR({
#' .(letrec, .(.(i, 3),
#'             .(foo, .(lambda, .(n), .(`+`, n, 1)))),
#'      .(`==`, i, .(foo, 2)))
#' }, pkg=TRUE)
#'
#' @export
lambda <-
function(params, ...)
{
    args <- eval(substitute(alist(...)))

    if(length(args) == 0)
        stop("Too few arguments to lambda")

    #This is easy; putting the formals together is harder, but that's done
    #in the infix() code
    body <- as.call(c(list(as.symbol("{")), args))

    fn <- eval(call("function", as.pairlist(params), body), envir=parent.frame())
    environment(fn) <- parent.frame()

    fn
}

#' Compose functions
#'
#' Function composition is a common operation in functional programming.
#' Given some number of input functions, these functions return new functions
#' which represents the composition of the inputs. That is, for three functions
#' f, g, h, \code{compose(f, g, h)} returns a function equivalent for all x to
#' f(g(h(x))). \code{composeM} handles the case of functions of multiple
#' arguments.
#'
#' For \code{compose}, the functions passed must all take a single argument.
#'
#' For \code{composeM}, the functions passed must take n >= 1 arguments, and
#' return a list or vector. \code{composeM}'s returned function proceeds right
#' to left through the functions it composes, applying each to the sequence of
#' arguments returned by the previous function (via \code{\link{do.call}}). The
#' first function is called with the actual arguments passed.
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
#' @seealso
#' \code{\link{curry}}, \code{\link{lazy.curry}} and \code{\link{uncurry}},
#' which are frequently useful in conjunction with \code{compose}.
#'
#' @examples
#' f <- function(x) x+1
#' g <- function(x) 3*x
#' h <- function(x) x^2
#'
#' compose(f, g, h)(2) #=> 13 == 3(2)^2 + 1
#' @rdname compose
#' @name compose
#' @export
compose <-
function(..., where=parent.frame())
{
    nms <- eval(substitute(alist(...)))

    #Create the composed function
    helper <- function(f, g) function(x) f(g(x))
    fn <- Reduce(helper, resolv(fns=list(...), envir=where))

    #Set its srcref attribute
    txt <- "x"
    for(nm in rev(nms))
        txt <- c(nm, "(", c(txt, ")"))
    txt <- c("function(x) ", txt)
    txt <- paste0(txt, collapse="")

    s <- srcfilecopy("dummy.R", txt)
    sr <- srcref(s, c(1, 1, 1, nchar(txt)))

    attr(fn, "srcref") <- sr

    fn
}

#' @rdname compose
#' @export
composeM <-
function(..., where=parent.frame())
{
    nms <- eval(substitute(alist(...)))

    #Create the composed function
    helper <-
    function(f, g)
    {
        function(...) do.call(f, as.list(do.call(g, list(...))))
    }
    fn <- Reduce(helper, resolv(fns=list(...), envir=where))

    #Set its srcref attribute
    txt <- "list(...)"
    for(nm in rev(nms))
        txt <- c("do.call(", nm, ", as.list(", c(txt, "))"))
    txt <- c("function(...) ", txt)
    txt <- paste0(txt, collapse="")

    s <- srcfilecopy("dummy.R", txt)
    sr <- srcref(s, c(1, 1, 1, nchar(txt)))

    attr(fn, "srcref") <- sr

    fn
}

#' Partial function application
#'
#' \code{curry} and related functions modify other functions by pre-setting
#' their arguments.
#'
#' Strictly speaking, these functions do partial application rather than
#' currying, but they can be used to implement proper currying if desired, and
#' the abuse of terminology is common. For an introduction to the whole concept
#' of function currying and why it's useful, in more detail than we can give
#' here, see the ever-helpful
#' \href{https://en.wikipedia.org/wiki/Currying}{Wikipedia}.

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
#' @seealso
#' \code{\link{compose}}, which is frequently useful in conjunction with the
#' currying and uncurrying functions.
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
    nm <- deparse(substitute(f), backtick=TRUE)
    lst <- list(...)

    #Straight out of a CS textbook here, but uncurry() is a bit
    #more interesting
    func <- function(...) do.call(f, c(lst, list(...)))
    func <- structure(func, .curried=TRUE) #we don't need to track how many times

    #Set its srcref attribute
    txt <- paste0("function(...) do.call(", nm, ", c(", lst,
                  ", list(...)))", collapse="")

    s <- srcfilecopy("dummy.R", txt)
    sr <- srcref(s, c(1, 1, 1, nchar(txt)))

    attr(func, "srcref") <- sr

    func
}

#' @rdname curry
#' @export
lazy.curry <-
function(f, ...)
{
    nm <- deparse(substitute(f), backtick=TRUE)
    lst <- eval(substitute(alist(...)))

    func <- function(...) do.call(f, c(lst, list(...)))
    func <- structure(func, .curried=TRUE)

    #Set its srcref attribute
    txt <- paste0("function(...) do.call(", nm, ", c(", lst,
                  ", list(...)))", collapse="")

    s <- srcfilecopy("dummy.R", txt)
    sr <- srcref(s, c(1, 1, 1, nchar(txt)))

    attr(func, "srcref") <- sr

    func
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

#' Conjunction and disjunction
#'
#' \code{conjunct} and \code{disjunct} construct and return functions which
#' represent the conjunction or disjunction of predicates.
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
#' @param ... Predicate functions whose conjunction or disjunction to return.
#' @param where An environment in which to resolve any character or
#' symbol arguments.
#'
#' @return A function of a single argument, representing the conjunction or
#' disjunction of the predicates' values on its argument.
#'
#' @seealso
#' \code{compose}, which returns the composition rather than the conjunction or
#' disjunction.
#'
#' @examples
#' f <- conjunct(is.positive, is.odd)
#' g <- conjunct(is.positive, is.zero)
#' h <- disjunct(is.negative, is.odd)
#'
#' f(3) == TRUE
#' f(2) == FALSE
#'
#' #false for all x - no positive numbers are 0
#' g(0) == FALSE
#' g(10) == FALSE
#'
#' h(10) == FALSE
#' h(-10) == TRUE
#' h(3) == TRUE
#'
#' @rdname connectives
#' @name connectives
#' @export
conjunct <-
function(..., where=parent.frame())
{
    helper <- function(f, g) function(x) f(x) && g(x)
    Reduce(helper, resolv(fns=list(...), envir=where))
}

#' @rdname connectives
#' @export
disjunct <-
function(..., where=parent.frame())
{
    helper <- function(f, g) function(x) f(x) || g(x)
    Reduce(helper, resolv(fns=list(...), envir=where))
}

#' Searching for tails of sequences
#'
#' \code{member.if} and \code{member.if.not} search "sequences" (by which we
#' mean lists, other vectors or pairlists) for the first element satisfying
#' some predicate function, and return the sub-sequence beginning with that
#' element. \code{member.if.not} uses Negate(f) as its predicate.
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
#' @seealso
#' The functional-programming functions in base, especially \code{Filter},
#' under \code{\link[=Filter]{funprog}}.
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
        if( compose(f, k)(x[[i]]) )
        {
            if(is.pairlist(x))
                return(as.pairlist(x[i:length(x)]))
            else
                return(x[i:length(x)])
        }
    }

    return(NULL)
}

#' @rdname member-if
#' @export
member.if.not <-
function(f, x, k=identity)
{
    return(member.if(f=do.call(Negate, list(f)), x=x, k=k))
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
#' If not all sequences are the same length, shorter sequences are recycled
#' to match the lengths of longer ones.
#'
#' For those familiar with Python, this is just like the built-in Python
#' \code{zip} function.
#'
#' @param ... Sequences to zip.
#'
#' @return The zipped sequences.
#'
#' @seealso
#' \code{link{for.each}} for application of an arbitrary function to zipped
#' sequences of values.
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
    args <- list(...)

    if(length(args) == 0)
        return(list())

    if(min(vapply(args, length, numeric(1))) == 0)
        return(list())

    lst <- list(function(...) { return(list(...)) })
    lst <- c(lst, args)

    return(do.call(curry(mapply, SIMPLIFY=FALSE), lst))
}

#' Zipped function application
#'
#' \code{for.each} takes a function and a set of sequences (vectors, lists or
#' pairlists), and applies the function successively to each set of elements
#' that are in the sequences at the same positional index.
#'
#' This function combines \code{link{zip}} and \code{link{do.call}}, to apply
#' a provided function successively to the elements of several sequences. The
#' arity of the function must be compatible with the number of sequences
#' passed as arguments; if not, an error will result.
#'
#' As in \code{\link{zip}}, if not all sequences are the same length, shorter
#' sequences will be recycled to match the lengths of longer ones.
#'
#' @param f A function to apply.
#' @param ... Vectors, lists or pairlists.
#'
#' @return The value of f on the last set of parameters supplied to it, or
#' NULL if no or only zero-length sequences were supplied as arguments.
#'
#' @seealso
#' \code{link{zip}} for the operation of zipping the parameter sequences.
#'
#' @examples
#' f <- function(...) print(as.numeric(list(...)))
#' invisible(for.each(f, 1:5, 6:10, 11:15))
#'
#' @export
for.each <-
function(f, ...)
{
    params <- do.call(zip, list(...))

    ret <- NULL
    for(p in params)
    {
        ret <- do.call(f, p)
    }

    return(ret)
}

#' Additional higher-order functions
#'
#' These functions are aliases for or thin wrappers around the functions in
#' the base R \code{\link[=Filter]{funprog}} set. The aliases make the
#' functions available under the names that are usual in Lisp/Scheme.
#'
#' The following functions are aliases for functions from base:
#' \itemize{
#' \item{\code{map} is base's \code{\link{Map}};}
#' \item{\code{reduce} is base's \code{\link{Reduce}};}
#' \item{\code{keep.matching} is base's \code{\link{Filter}};}
#' }
#'
#' The \code{delete.matching} function does the opposite of
#' \code{keep.matching} and returns only elements of x for which f does not
#' return a logically true value.
#'
#' @param f A function of the appropriate arity, as in
#' \code{\link[=Filter]{funprog}}.
#' @param x A vector.
#' @param ... As in \code{link{Map}}.
#' @param init As in \code{link{Reduce}}.
#' @param right As in \code{link{Reduce}}.
#' @param accumulate As in \code{link{Reduce}}.
#'
#' @return \code{delete.matching} returns the elements of x for which f is not
#' logically true.
#'
#' @seealso
#' The functional-programming functions in base, especially \code{Filter},
#' under \code{\link[=Filter]{funprog}}.
#'
#' @rdname funprog-extra
#' @name funprog-extra
#' @export
delete.matching <-
function(f, x)
{
    return(Filter(Negate(f), x))
}

#' @rdname funprog-extra
#' @export
reduce <- Reduce

#' @rdname funprog-extra
#' @export
map <- Map

#' @rdname funprog-extra
#' @export
keep.matching <- Filter
