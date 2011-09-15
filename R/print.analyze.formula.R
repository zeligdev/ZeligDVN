#' Print the Analysis of a \code{formula}
#'
#' @usage \method{print}{analyze.formula}(x, digits=3, ...)
#' @S3method print analyze.formula
#' @param x a \code{analyze.formula} object
#' @param digits an integer specifying the precision of numeric variables
#' @param ... ignored parameters
#' @return the original object (invisibly)
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.analyze.formula <- function (x, digits=3, ...) {

  size <- length(x)

  for (key in names(x)) {

    # Print term name
    cat("Term Name:", key, "\n")
    print(x[[key]], digits=digits, ...)

    # Print new-lines between all elements
    if (size <- size - 1)
      cat("\n")

  }

  invisible(x)
}
