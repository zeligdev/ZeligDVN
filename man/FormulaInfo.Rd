\name{FormulaInfo}
\alias{FormulaInfo}
\title{Summarize Important Information of a Formula...}
\usage{FormulaInfo(formula, ...)}
\description{Summarize Important Information of a Formula}
\details{This function extracts important information from a formula object. In
particular it provides completes lists comprising the entire set of
variables, functions applied to these variables, and independent expressions
within the formula.}
\value{a \code{FormulaInfo} containg the following indices:
\item{\code{vars}}{a list of symbolic names of variables in the 
in the formula}
\item{\code{functions}}{a list of symblic names of fucntions used within
the formula}
\item{\code{entries}}{the list of symbolic expressions used within the
first-level of the equation. For example, the formula 
\code{response ~ s(t) + sin(t) + t} 
would contain an entries field with: 
\code{response}, \code{s(t)}, \code{t}}}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{formula}{a \code{formula} object}
\item{...}{ignored parameters}}
