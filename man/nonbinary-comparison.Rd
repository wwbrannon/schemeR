% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic.R
\name{nonbinary-comparison}
\alias{eq}
\alias{ge}
\alias{gt}
\alias{le}
\alias{lt}
\alias{nonbinary-comparison}
\title{Non-binary comparison operators}
\usage{
eq(...)

ge(...)

le(...)

gt(...)

lt(...)
}
\arguments{
\item{...}{Values to compare.}
}
\value{
The boolean result of the comparison.
}
\description{
These functions provide versions of R's built-in binary comparison operators
that work with arbitrarily many arguments. \code{ge} is the \code{>=}
operator, \code{le} is \code{<=}, \code{eq} is \code{==}, \code{lt} is
\code{<}, and \code{gt} is \code{>}.
}
\details{
If more than 2 arguments are passed, the result is the logical conjunction
of the binary operator's return value for each pair of arguments (in order).
For example, le(1,2,3) is equivalent to 1 <= 2 <= 3 (mathematically
speaking; the second of those is not a valid R expression). In R,
le(1,2,3) is equivalent to 1 <= 2 && 2 <= 3, and so on for more arguments.
}
\examples{
le(1,2,3) == TRUE

ge(1,2,3) == FALSE

eq(1,1,1) == TRUE

eq(1,2,3) == FALSE

}

