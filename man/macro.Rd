% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/defmacro.R
\name{macro}
\alias{defmacro}
\alias{macro}
\title{Lisp macros for R}
\usage{
defmacro(nm, params, ...)

macro(params, ...)
}
\arguments{
\item{nm}{The symbol to which \code{defmacro} should bind the generated
macro.}

\item{params}{A parameter list, which when coerced to pairlist is acceptable
to the "function" constructor function.}

\item{...}{Body statements.}
}
\value{
\code{macro} returns the created macro, which is an R function.
\code{defmacro}, as in Common Lisp, returns the symbol it's bound the new
macro to.
}
\description{
These two functions create Lisp-style macros. \code{macro} creates and
returns anonymous macros; defmacro is a convenience wrapper around
\code{macro} which creates a macro and assigns it to a name in the current
environment. A full discussion of macros and how to use them is beyond the
scope of this page; see the vignettes for an R-focused introduction.
}
\details{
Even though R is descended from Scheme, the macros implemented here are
based on the "non-hygienic" ones used in Common Lisp. They contrast with
the hygienic form that's traditional in Scheme, which is based on syntax
objects and the syntax-case special form.
}
\seealso{
The \code{\link{gensym}} function, which generates temporary unique symbols
that macro definitions can use to avoid capturing variables from the calling
environment.
}

