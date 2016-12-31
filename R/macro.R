## A Lisp-style macro facility for R

#' @export
. <-
function(f, ...)
{
    match.call()[-1]
}

reval <-
function(expr)
{

}

#' @export
macroProcess <-
function(files)
{
    filename <- 'R/foo.Rl'
    txt <- readChar(filename, file.info(filename)$size)
}

