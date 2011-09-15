#' Print the Analysis of a \code{formula}
#'
#' @S3method print print analyze.formula
#' @param x a \code{data.frame} analysis object
#' @param digits an integer specifying the precision of numeric variables
#' @param ... ignored parameters
#' @return the original object (invisibly)
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.analyze.formula <- function (x, digits, ...) {

  size <- length(x)

  for (key in names(x)) {

    # Print term name
    cat("Term Name:", key, "\n")
    print(x[[key]], ...)

    # Print new-lines between all elements
    if (size <- size - 1)
      cat("\n")

  }

  invisible(x)
}
