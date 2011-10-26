\name{write.script}
\alias{write.script}
\title{Write Script for DVN...}
\usage{write.script(model, outcome, explanatory, intercept=1, con=stdout())}
\description{Write Script for DVN}
\details{This function creates a script (as a string) to compute a statistical model
for use with the Dataverse.}
\value{nothing - this function is used for its spillover}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{model}{a character-string specifying the model to compute}
\item{outcome}{a character-vector specifying outcome variables}
\item{explanatory}{a character-vector or list of character-vectors
specifying explanatory variables for each outcome variable}
\item{intercept}{the intercept parameter - setting this value to 0 will 
fit the statistical model without using an intercept}}
