\name{info.matrix}
\alias{info.matrix}
\title{Get Information about an Atomic Matrix...}
\usage{info.matrix(x, ..., threshold)}
\description{Get Information about an Atomic Matrix}
\details{This method facilitates the extraction of meaningful information from a
an atomic *matrix* extracted from a \code{formula} and \code{data.frame}.}
\note{This function is primarily used internally by the various 
\code{analyze} methods.}
\value{an \code{info.atomic} object}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{x}{a matrix containing atomic values}
\item{...}{ignored parameters}
\item{threshold}{an integer specifying the maximum size of allowing numeric
data to be considered potential factor data. That is, if a data.frame
contains a numeric column with less unique elements than the value set by
'threshold', then this method will consider it valid factor-style data.}}
