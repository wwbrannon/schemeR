## Allow writing R code in an infix style, like Lisp

#' @export
. <-
function(f, ...)
{
    match.call()[-1]
}

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

expr2char <-
function(expr)
{
    #We don't want to handle expressions nested under calls: the idea here
    #is to handle only the type of expressions that result from infix()
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

#' @export
infixFiles <-
function(files, overwrite=FALSE)
{
    #If !overwrite, we're going to assume each of these not-quite-R
    #files has the extension ".Rl" so we can write the converted file
    #with extension ".R" - throw if this is not the case
    if(!is.null(overwrite) && !overwrite)
    {
        exts <- unique(vapply(files, tools::file_ext, character(1)))
        if(length(exts) > 1 || exts[1] != "Rl")
        {
            stop("Files must have .Rl extension unless overwrite=TRUE")
        }
    }

    for(filename in files)
    {
        filename <- normalizePath(filename)

        txt <- readChar(filename, file.info(filename)$size)
        conv <- infix(parse(text=txt))

        if(!is.null(overwrite) && overwrite)
        {
            nf <- filename
        } else
        {
            ext <- tools::file_ext(filename)
            nf <- paste0(substr(filename, 1, nchar(filename) - nchar(ext) - 1), ".R")
        }

        con <- file(nf, open="w+")
        cat(expr2char(conv), file=con)

        return(invisible(NULL))
    }
}
