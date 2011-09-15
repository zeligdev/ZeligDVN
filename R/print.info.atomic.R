#' Print a Column Analysis
#'
#' Correctly format and display the analysis of an individual column.
#' @S3method print info.atomic
#' @param x a \code{column.analysis} object
#' @param digits a numeric specifying the number of digits of precision to use
#'   for numeric values
#' @param ... ignored parameters
#' @return the object (invisibly)
print.info.atomic <- function (x, digits, ...) {
  cat("Class: ", x$class, "\n")
  cat("Mode:  ", x$mode, "\n")
  cat("Length:", x$length, "\n")
  cat("Unique Values:", length(x$unique), "\n")

  invisible(x)
}
