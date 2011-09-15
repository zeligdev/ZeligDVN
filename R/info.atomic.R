#' Get Information about an Atomic Vector
#'
#' This method facilitates the extraction of meaningful information from a
#' an atomic *vector* extracted from a \code{data.frame}.
#' @note This function is primarily used internally by the various 
#'   \code{analyze} methods.
#' @param x an atomic vector
#' @param ... ignored parameters
#' @param threshold an integer specifying the maximum size of allowing numeric
#'   data to be considered potential factor data. That is, if a data.frame
#'   contains a numeric column with less unique elements than the value set by
#'   'threshold', then this method will consider it valid factor-style data.
#' @return an \code{info.atomic} object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
info.atomic <- function(x, ..., threshold=100) {
  obj <- list(
              summary = summary(x),
              length  = length(x),
              unique  = unique(x),
              mode    = mode(x),
              class   = class(x),
              call    = match.call()
              )

  attr(obj, 'levels') <- levels(x)
  class(obj) <- "info.atomic"

  obj
}
