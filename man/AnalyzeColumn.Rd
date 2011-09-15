\name{AnalyzeColumn}
\alias{AnalyzeColumn}
\title{Analyze a Column of a data...}
\usage{AnalyzeColumn(obj, column, threshold=100)}
\description{Analyze a Column of a data.frame}
\details{Analyze a column of a data.frame and return important information relevant
to dataverse analysises. This is a necessary component for analyzing entire
data.frame's.}
\value{an object}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{obj}{a data.frame}
\item{column}{a character-string specifying a column of data}
\item{threshold}{an integer specifying the maximum size of allowing numeric
data to be considered potential factor data. That is, if a data.frame
contains a numeric column with less unique elements than the value set by
'threshold', then this method will consider it valid factor-style data.}}
