#' Print a Column Analysis
#'
#' Correctly format and display the analysis of an individual column.
#' @usage \method{print}{info.atomic}(x, ...)
#' @S3method print info.atomic
#' @param x a \code{column.analysis} object
#' @param ... ignored parameters
#' @return the object (invisibly)
print.info.atomic <- function (x, ...) {
  cat("Class: ", x$class, "\n")
  cat("Mode:  ", x$mode, "\n")
  cat("Length:", x$length, "\n")
  cat("Unique Values:", length(x$unique), "\n")

  invisible(x)
}
