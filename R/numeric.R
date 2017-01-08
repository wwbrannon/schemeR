## Numeric functions
expt <- `^`
is.number <- is.numeric
quotient <- `%/%`
remainder <- `%%`
modulo <- remainder

sum <- function(...) { Reduce(`+`, list(...)) }
is.even <- function(x) { return(x %% 2 == 0) }
is.odd  <- function(x) { return(x %% 2 == 1) }
is.zero <- function(x) { return(x == 0) }
is.positive <- function(x) { return(x > 0) }
is.negative <- function(x) { return(x < 0) }

