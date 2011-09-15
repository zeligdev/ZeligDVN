#' Print the Analysis of a \code{list}
#'
#' @S3method print analyze.list
#' @param x a \code{analyze.list} analysis object
#' @param digits an integer specifying the precision of numeric variables
#' @param ... ignored parameters
#' @return the original object (invisibly)
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.analyze.list <- function (x, digits, ...) {
  size <- length(x)

  for (key in names(x)) {

    # Print term name
    cat("> Equation Name:", key, "\n")
    print(x[[key]], ...)

    # Print new-lines between all elements
    if (size <- size - 1)
      cat("\n")

  }

  invisible(x)

}
