\name{check.negative}
\alias{check.negative}
\title{Determine Whether Expression is Negative...}
\usage{check.negative(expr, data)}
\description{Determine Whether Expression is Negative}
\details{This defines what a negative piece of data is. In this case, simply not
being continuous is sufficient.}
\value{a logical specifying whether the given expression produces binary
data as a result}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{expr}{an expression representing a data column}
\item{data}{a \code{data.frame} used to evaluate \code{column}}}
