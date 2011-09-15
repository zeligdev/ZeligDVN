#' Get Information about an Atomic Matrix
#'
#' This method facilitates the extraction of meaningful information from a
#' an atomic *matrix* extracted from a \code{formula} and \code{data.frame}.
#' @note This function is primarily used internally by the various 
#'   \code{analyze} methods.
#' @param x a matrix containing atomic values
#' @param ... ignored parameters
#' @param threshold an integer specifying the maximum size of allowing numeric
#'   data to be considered potential factor data. That is, if a data.frame
#'   contains a numeric column with less unique elements than the value set by
#'   'threshold', then this method will consider it valid factor-style data.
#' @return an \code{info.atomic} object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
info.matrix <- function(x, ..., threshold) {

  cols <- names(colnames(x))
  results <- NULL

  for (k in 1:ncol(x)) {
    col.info <- info.atomic(x[, k])
    results <- cbind(results, col.info)
  }
  
  colnames(results) <- NULL
  class(results) <- "info.matrix"
  results
}
