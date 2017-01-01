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

#' @export
infixFiles <-
function(files, overwrite=FALSE)
{
    fileconv(files=files, direction="infix", overwrite=overwrite)
}

#' @export
prefixFiles <-
function(files, overwrite=FALSE)
{
    fileconv(files=files, direction="prefix", overwrite=overwrite)
}
