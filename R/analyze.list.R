#' Analyze a List
#'
#' This function analyzes the types of data used in a formula.
#' @param obj a \code{list} object of analyzable data
#' @param data a \code{data.frame} 
#' @return a \code{data.frame.analysis} object
#' @author Matt Owen \email{mowen@@iq.harvard.ed}
#' @export
analyze.list <- function (obj, data, ...) {

  results <- list()
  NAMES <- names(obj)

  for (k in 1:length(obj)) {
    key <- NAMES[[k]]

    if (is.null(key) || nchar(key) == 0)
      key <- paste("Equation #", k, sep="")

    results[[key]] <- analyze(obj[[k]], data=data)
  }

  class(results) <- "analyze.list"

  results
}
