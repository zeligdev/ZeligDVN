#' Analyze a Formula
#'
#' This function analyzes the types of data used in a formula, using a 
#' \code{data.frame} as reference.
#' @param obj a \code{formula}
#' @param data a \code{data.frame}
#' @return a \code{data.frame.analysis} object
#' @author Matt Owen \email{mowen@@iq.harvard.ed}
#' @export
analyze.formula <- function (obj, data, ...) {

  # Acquire information about the formula
  formula.info <- formula.info(obj)

  # Extract variables and terms
  vars <- formula.info[["vars"]]
  entries <- formula.info[["entries"]]

  # Create a unique list of values
  all.entries <- unique(append(entries, vars))

  # Init a list to store all the results
  results <- list()

  # This loop will analyze each individual term.
  for (expr in all.entries) {
    # Evaluate expression within context of the data-set
    val <- eval(expr, data)

    # Analyze the resulting object
    result <- if (is.matrix(val))
      info.matrix(val)
    else if (is.atomic(val))
      info.atomic(val)
    else
      NA

    # Retrieve the name of the call
    name <- as.expression(expr)
    name <- as.character(name)

    # Add to list of results
    results[[name]] <- result

    # Just to make sure
    rm(result)
  }

  class(results) <- "analyze.formula"

  results
}
