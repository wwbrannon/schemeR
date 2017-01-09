## Numeric functions

#' @export
expt <- `^`

#' @export
is.number <- is.numeric

#' @export
quotient <- `%/%`

#' @export
remainder <- `%%`

#' @export
modulo <- remainder

#' @export
is.even <-
function(x)
{
    return(x %% 2 == 0)
}

#' @export
is.odd <-
function(x)
{
    return(x %% 2 == 1)
}

#' @export
is.zero <-
function(x)
{
    return(x == 0)
}

#' @export
is.positive <-
function(x)
{
    return(x > 0)
}

#' @export
is.negative <-
function(x)
{
    return(x < 0)
}
