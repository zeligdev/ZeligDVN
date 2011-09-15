#' Print Information about a Matrix
#'
#' Correctly format and display the analysis of an individual column.
#' @S3method print info.matrix
#' @param x a \code{column.analysis} object
#' @param digits a numeric specifying the number of digits of precision to use
#'   for numeric values
#' @param ... ignored parameters
#' @return the object (invisibly)
print.info.matrix <- function (x, digits=3, ...) {
  CLASS <- MODE <- LENGTH <- UNIQUE <- NULL

  display.matrix <- matrix(NA, 4, ncol(x))
  rownames(display.matrix) <- c("Class", "Mode", "Length", "Unique")
  colnames(display.matrix) <- paste("column", 1:ncol(x), sep="=")

  # Get all values
  for (k in 1:ncol(x)) {
    column <- x[, k]
    display.matrix["Class", k] <- column$class
    display.matrix["Mode", k] <- column$mode
    display.matrix["Length", k] <- column$length
    display.matrix["Unique", k] <- length(column$unique)
  }

  print(display.matrix, digits=digits, quote=FALSE, justify="right")

  invisible(x)
}
