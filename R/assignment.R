## Assignment operators
## Unlike in Scheme, there's no distinction between creating a binding
## and assigning a value, so define <=> set <=> `<-`.
#' @export
set <- `<-`

#' @export
set.pos <-
function(nm, pos, val)
{
    target <- as.symbol(deparse(substitute(nm)))

    if(is.list(nm))
    {
        expr <- bquote(.(target)[[.(pos)]] <- .(val))
    } else
    {
        expr <- bquote(.(target)[.(pos)] <- .(val))
    }

    eval(expr, envir=parent.frame())
}

#' @export
set.car <- function(nm, val) set.pos(nm=nm, val=val, pos=1)

#' @export
set.cdr <-
function(nm, val)
{
    target <- as.symbol(deparse(substitute(nm)))

    if(is.list(nm))
    {
        expr <- bquote(.(target) <- c(.(target)[1], .(val)))
    } else
    {
        expr <- bquote(.(target)[2:length( .(target) )] <- .(val))
    }

    eval(expr, envir=parent.frame())
}
