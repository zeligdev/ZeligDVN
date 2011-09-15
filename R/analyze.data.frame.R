#' Analyze a \code{data.frame}
#'
#' This method analyzes 
#' @param obj a \code{data.frame}
#' @param ... additional parameters
#' @return an object describing
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
analyze.data.frame <- function (obj, ...) {

  results <- list()

  for (column in colnames(obj))
    results[[column]] <- AnalyzeColumn(obj, column)

  attr(results, 'label') <- substitute(obj)
  attr(results, 'call') <- match.call()
  attr(results, 'parent') <- parent.frame()

  class(results) <- "analyze.data.frame"

  results
}
