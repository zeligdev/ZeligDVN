\name{split.pkg}
\alias{split.pkg}
\title{Split Package Version String into Component Parts...}
\usage{split.pkg(string)}
\description{Split Package Version String into Component Parts}
\details{This function splits a character-string specifying package dependency into 
its three parts: package name, version number and operator specifying
whether its dependency is an inequality.}
\note{This function is used internally by Zelig.}
\value{a character-vector with three names components: name, operator and
number, respectively corresponding to package name, equality operator and 
version number (exact or based on operator).}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{string}{a character-string formatted like a dependency requirement.
E.g., "Package (>= 3.0)", "Package", "Package (1.0)"}}
