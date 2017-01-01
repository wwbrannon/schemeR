## Various basic Scheme functions implemented for R. Unlike the versions in
## the "lisp" package, these work on both lists and vectors, and include more
## functions.

lambda <- `function`
define <- `<-`
map <- Map
display <- print
cons <- c

nil <- NULL

first <-
function(obj)
{
    if(is.list(obj) || is.pairlist(obj))
    {
        return(obj[[1]])
    } else
    {
        return(obj[1])
    }
}

last <-
function(obj)
{
    n <- length(obj)

    if(is.list(obj) || is.pairlist(obj))
    {
        return(obj[[n]])
    } else
    {
        return(obj[n])
    }
}

