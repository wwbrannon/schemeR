## Some basic / misc renamings of things R already has
#' @export
nil <- NULL

#' @export
display <- print

#' @export
progn <- `{`

#' @export
fromPkg <- `::`

#' @export
is.nil <- is.null

#' @export
is.procedure <- is.function

#' @export
is.boolean <- is.logical

## Non-binary comparison operators
## eq, ge, le, gt, lt: =, >=, <=, >, <
#' @export
eq <- function(...) { Reduce(`==`, list(...)) }

#' @export
ge <- function(...) { Reduce(`>=`, list(...)) }

#' @export
le <- function(...) { Reduce(`<=`, list(...)) }

#' @export
gt <- function(...) { Reduce(`>`, list(...)) }

#' @export
lt <- function(...) { Reduce(`<`, list(...)) }
