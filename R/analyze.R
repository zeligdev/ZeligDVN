#' Generic Method for Analyzing R-objects for Use with DVN
#'
#' This method provides summary information that will be used to categorize
#' how and where particular types of data can be used with the DVN.
#' @usage
#'   analyze(obj, ...)
#'   analyze(formula, data, threshold)
#' @export analyze
#' @param obj 
#' @param ...
#' @return an \code{analyzed} object
analyze <- function(obj, ...)
  UseMethod("analyze")
