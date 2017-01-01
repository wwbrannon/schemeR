## Allow writing R code in a prefix style, like Lisp

#' Facilities for writing R code in prefix notation
#'
#' \code{prefix} and \code{infix} convert R code between the usual infix
#' syntax and the Lisp-like prefix format written via the \code{\link{.}}
#' function (see below). A call to \code{prefix()} does not evaluate the passed
#' or the generated expressions, partly for efficiency reasons; \code{infix()},
#' though it still does not evaluate the generated expression, does do
#' partial evaluation of the passed expression: each call to \code{.} is
#' evaluated to the corresponding infix call.
#'
#' @param expr A prefix- or infix-formatted expression to convert
#'
#' @return \code{prefix} and \code{infix} return the input expression
#' converted to prefix or infix.
#'
#' @section See Also:
#' \code{\link{prefixFiles}} and \code{\link{infixFiles}} for converting
#' code found in files.
#'
#' @name prefix
#' @rdname prefix
#' @export
prefix <-
function(expr)
{
    if(is.expression(expr))
    {
        lst <- lapply(as.list(expr), prefix)
        return(as.expression(lst))
    } else if(is.call(expr))
    {
        lst <- lapply(as.list(expr), prefix)
        lst <- c(as.symbol("."), lst)

        return(as.call(lst))
    } else
    {
        return(expr)
    }
}

#' @rdname prefix
#' @export
infix <-
function(expr)
{
    if(is.expression(expr))
    {
        lst <- lapply(as.list(expr), infix)
        return(as.expression(lst))
    } else if(is.call(expr))
    {
        if(expr[[1]] == as.symbol("."))
        {
            env <- new.env(parent=emptyenv())
            assign(".", `.`, envir=env)

            expr <- eval(expr, envir=env)
        }

        lst <- lapply(as.list(expr), infix)
        return(as.call(lst))
    } else
    {
        return(expr)
    }
}

#' The \code{.} function allows writing an R function call in a Lisp-like prefix
#' format, rather than the f(x, y, ...) infix format that R usually uses.
#' When evaluated, a call to \code{.} doesn't actually execute the function
#' that's been expressed in prefix style, just generates the corresponding
#' infix call. It's intended as a notational aid, so that entire scripts
#' can be written in the prefix form.
#'
#' @param f The function that \code{.} should generate a call to.
#' @param ... Further arguments to the function f.
#'
#' @return \code{.} returns an unevaluated call to f with \code{...} arguments
#' carried through.
#'
#' @rdname prefix
#' @export
. <-
function(f, ...)
{
    match.call()[-1]
}

# Convert an R expression object to a format suitable for writing to a file
#
# This function converts the sort of R object that results from a call to
# infix() or prefix() to a character string in a format suitable for writing
# to a file with cat(). The code written should be executable directly when
# read back in from the file, rather than being wrapped in an expression
# object.
#
# @param expr An R object.
#
# @return The string representation of expr.
expr2char <-
function(expr)
{
    #We don't want to handle expressions nested under calls: the idea here
    #is to handle only the type of expressions that result from infix() and
    #prefix()
    if(is.expression(expr))
    {
        lst <- lapply(as.list(expr), expr2char)
        return(paste0(lst, collapse="\n"))
    } else
    {
        con <- textConnection(NULL, open="w")

        dput(expr, file=con)
        return(paste0(textConnectionValue(con), collapse="\n"))
    }
}

# Convert one or more R code files from infix to prefix, or vice versa
#
# This function converts files from infix to prefix, or vice versa. It can
# be told to overwrite the passed files, or to generate new files. In the
# latter case, the input files are assumed to have extension ".R" for infix
# code, and ".Rl" for prefix code. For a given input file, the corresponding
# output file will have the other extension.
#
# @param files A character vector of file paths to convert.
# @param direction Either "infix" or "prefix" - what should we generate?
# @param overwrite Should the generated code overwrite the input file?
#
# @return Invisible NULL.
fileconv <-
function(files, direction, overwrite=FALSE)
{
    stopifnot(direction %in% c("prefix", "infix"))
    fn <- get(direction)

    if(is.null(overwrite) || !overwrite)
    {
        target_ext <- ifelse(direction == "prefix", "Rl", "R")
        src_ext <- ifelse(direction == "prefix", "R", "Rl")

        exts <- unique(vapply(files, tools::file_ext, character(1)))
        if(length(exts) > 1 || exts[1] != src_ext)
        {
            stop(paste0("Files must have .", src_ext, " extension unless overwrite==TRUE"))
        }
    }

    for(filename in files)
    {
        filename <- normalizePath(filename)

        txt <- readChar(filename, file.info(filename)$size)
        conv <- fn(parse(text=txt))

        if(is.null(overwrite) || !overwrite)
        {
            ext <- tools::file_ext(filename)
            nf <- paste0(substr(filename, 1, nchar(filename) - nchar(ext) - 1), ".")
            nf <- paste0(nf, target_ext)
        } else
        {
            nf <- filename
        }

        con <- file(nf, open="w+")
        cat(expr2char(conv), file=con)

        return(invisible(NULL))
    }
}

#' Convert R code files from prefix to infix, or vice versa
#'
#' These functions convert files from prefix to infix, and vice versa.
#' They can be told to overwrite the passed files, or to generate new files.
#' In the latter case, when converting to infix, the input files are assumed
#' to have extension ".Rl", and the corresponding output files will have
#' extension ".R"; when converting to prefix, the input files are assumed
#' to have extension ".R", and the corresponding output files will have
#' extension ".Rl".
#'
#' @param files A character vector of file paths to convert.
#' @param overwrite Should the generated code overwrite the input file?
#'
#' @return Invisible NULL.
#'
#' @section See Also:
#' \code{\link{infix}} and \code{\link{prefix}} for converting expressions
#' from R-style infix to Lisp-style prefix and vice versa; the \code{\link{.}}
#' function, which is the building block of prefix-formatted R code.
#'
#' @name fileconv
#' @rdname fileconv
#' @export
prefixFiles <-
function(files, overwrite=FALSE)
{
    fileconv(files=files, direction="prefix", overwrite=overwrite)
}

#' @rdname fileconv
#' @export
infixFiles <-
function(files, overwrite=FALSE)
{
    fileconv(files=files, direction="infix", overwrite=overwrite)
}
