\name{check.positive}
\alias{check.positive}
\title{Determine Whether Expression is Positive...}
\usage{check.positive(expr, data)}
\description{Determine Whether Expression is Positive}
\details{This defines what a discrete piece of data is. In this case, simply not
being continuous is sufficient.}
\value{a logical specifying whether the given expression produces binary
data as a result}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{expr}{an expression representing a data column}
\item{data}{a \code{data.frame} used to evaluate \code{column}}}
