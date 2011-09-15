\name{analyze}
\alias{analyze}
\title{Generic Method for Analyzing R-objects for Use with DVN...}
\usage{analyze(obj, ...)}
\description{Generic Method for Analyzing R-objects for Use with DVN}
\details{This method provides summary information that will be used to categorize
how and where particular types of data can be used with the DVN.}
\value{an analyzed object. The class may vary depending the object type.}
\seealso{analyze.data.frame analyze.list analyze.formula}
\arguments{\item{obj}{an object to analyze for Zelig-to-Dataverse connectivity. The
resulting object will contain information concerning the kind of data it
contains, quantity of unique values, etc.}
\item{...}{optional parameters}}
