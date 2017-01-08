## Some basic / misc renamings of things R already has
nil <- NULL
define <- `<-`
display <- print
progn <- `{`
fromPkg <- `::`

is.nil <- is.null
is.procedure <- is.function
is.boolean <- is.logical

## Non-binary comparison operators
## eq, ge, le, gt, lt: =, >=, <=, >, <
eq <- function(...) { Reduce(`==`, list(...)) }
ge <- function(...) { Reduce(`>=`, list(...)) }
le <- function(...) { Reduce(`<=`, list(...)) }
gt <- function(...) { Reduce(`>`, list(...)) }
lt <- function(...) { Reduce(`<`, list(...)) }

