## A Lisp-like macro facility for R

## Because normal Lisps have so much more permissive rules about what is and
## isn't a syntactic name, we can't use exactly the same names for quote,
## backquote, comma and comma-at. But it's extremely convenient to be able to
## refer to these things with short syntactic constructs, especially in macro
## definitions, so we'll give them alternate names:
##     quote: .q
##     backquote: .b
##     comma: .c
##     comma-at: .s (for "splice")
## and use these names in lieu of the "." that otherwise begins all lists in
## our prefix syntax.
##
## Sharp-quote is unnecessary because, as in Scheme but contra Common Lisp,
## lambda is evaluable and evaluates directly to a function object.

#Build a temporary symbol, making sure it's not included in
#the list of bindings already in some environment, and prepend
#a prefix.
gensym <-
function(str="G", envir=parent.frame(), len=10)
{
    if(len <= 0)
        stop("Symbol length must be positive")

    lst <- as.list(envir)

    flchars <- c(letters, LETTERS)
    fl <- sample(flchars, 1)

    chars <- c(letters, LETTERS, vapply(0:9, as.character, character(1)))
    oc <- sample(chars, len - 1)

    nm <- paste0(c(fl, oc), collapse="")

    if(!is.null(lst))
    {
        repeat
        {
            if(!(nm %in% lst))
            {
                break
            }

            nm <- paste0(sample(chars, len), collapse="")
        }
    }

    return(as.symbol(paste0(str, nm)))
}

#defmacro
