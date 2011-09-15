#' Print the Analysis of a \code{list}
#'
#' @usage \method{print}{analyze.list}(x, digits=3, ...)
#' @S3method print analyze.list
#' @param x a \code{analyze.list} object
#' @param digits an integer specifying the precision of numeric variables
#' @param ... ignored parameters
#' @return the original object (invisibly)
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.analyze.list <- function (x, digits=3, ...) {
  size <- length(x)

  for (key in names(x)) {

    # Print term name
    cat("> Equation Name:", key, "\n")
    print(x[[key]], digits=3, ...)

    # Print new-lines between all elements
    if (size <- size - 1)
      cat("\n")

  }

  invisible(x)

}
