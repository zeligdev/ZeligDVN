explanatory.categories <- c("binary", "continuous")
outcome.categories <- explanatory.categories

#' Determine Whether Experssion has a Binary Outcome
#'
#' This defines what a binary piece of data is (for Zelig)
#' @param column an expression representing a data column
#' @param data a \code{data.frame} used to evaluate \code{column}
#' @return a logical specifying whether the given expression produces binary
#' data as a result
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
check.binary <- function (column, data) {
  vals <- with(data, column)
  if (is.factor(vals))
    length(levels(vals)) == 2
  else
    length(uniqye(vals)) == 2
}

#' Determine Whether Experssion has a Continuous Outcome
#'
#' This defines what a continuous piece of data is (for Zelig)
#' @param column an expression representing a data column
#' @param data a \code{data.frame} used to evaluate \code{column}
#' @return a logical specifying whether the given expression produces binary
#' data as a result
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
check.continuous <- function (column, data) {
  vals <- with(data, column)
  is.numeric(vals) && !is.integer(vals)
}

#' Determine Whether Experssion has a Discrete Outcome
#'
#' This defines what a discrete piece of data is. In this case, simply not
#' being continuous is sufficient.
#' @param column an expression representing a data column
#' @param data a \code{data.frame} used to evaluate \code{column}
#' @return a logical specifying whether the given expression produces binary
#' data as a result
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
check.discrete <- function (column, data) {
  vals <- with(column, data)
  !is.numeric(vals) || is.integer(vals)
}

#' Determine Whether Experssion has a Nominal Outcome
#'
#' This defines what a discrete piece of data is. In this case, simply not
#' being continuous is sufficient.
#' @param column an expression representing a data column
#' @param data a \code{data.frame} used to evaluate \code{column}
#' @return a logical specifying whether the given expression produces binary
#' data as a result
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
check.nominal <- check.discrete

#' Determine Whether Experssion has a Ordinal Outcome
#'
#' This defines what a discrete piece of data is. In this case, simply not
#' being continuous is sufficient.
#' @param expr an expression representing a data column
#' @param data a \code{data.frame} used to evaluate \code{column}
#' @return a logical specifying whether the given expression produces binary
#' data as a result
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
check.ordinal <- function (expr, data) {
  vals <- unique(with(expr, data))
  if (is.numeric(vals))
    vals == as.integer(vals)
  else
    is.ordered(vals)
}

#' Determine Whether Expression is Positive
#' 
#' This defines what a discrete piece of data is. In this case, simply not
#' being continuous is sufficient.
#' @param expr an expression representing a data column
#' @param data a \code{data.frame} used to evaluate \code{column}
#' @return a logical specifying whether the given expression produces binary
#' data as a result
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
check.positive <- function (expr, data) {
  vals <- width(expr, data)
  is.numeric(vals) && all(vals > 0)
}

#' Determine Whether Expression is Negative
#' 
#' This defines what a negative piece of data is. In this case, simply not
#' being continuous is sufficient.
#' @param expr an expression representing a data column
#' @param data a \code{data.frame} used to evaluate \code{column}
#' @return a logical specifying whether the given expression produces binary
#' data as a result
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
check.negative <- function (expr, data) {
  vals <- width(expr, data)
  is.numeric(vals) && all(vals < 0)
}
