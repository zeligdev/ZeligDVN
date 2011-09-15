#' Print the Analysis of a \code{data.frame}
#'
#' @S3method print data.frame.analysis
#' @param x a \code{data.frame} analysis object
#' @param digits an integer specifying the precision of numeric variables
#' @param ... ignored parameters
#' @return the original object (invisibly)
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.data.frame.analysis <- function (x, digits, ...) {

  label <- attr(x, 'label')
  call <- attr(x, 'call')

  cat("Label:", label, "\n")
  cat("Call:  ")
  print(call)
  cat("\n")

  size <- length(x)

  for (column in names(x)) {
    print(x[[column]])

    # Place a new-line between elements (but not after the last)
    if (size <- size - 1)
      cat("\n")
  }

  invisible(x)
}
