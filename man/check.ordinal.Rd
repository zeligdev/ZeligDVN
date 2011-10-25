\name{check.ordinal}
\alias{check.ordinal}
\title{Determine Whether Experssion has a Ordinal Outcome...}
\usage{check.ordinal(expr, data)}
\description{Determine Whether Experssion has a Ordinal Outcome}
\details{This defines what a discrete piece of data is. In this case, simply not
being continuous is sufficient.}
\value{a logical specifying whether the given expression produces binary
data as a result}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{expr}{an expression representing a data column}
\item{data}{a \code{data.frame} used to evaluate \code{column}}}
