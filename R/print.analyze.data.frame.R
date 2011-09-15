#' Print the Analysis of a \code{data.frame}
#'
#' @usage \method{print}{analyze.data.frame}(x, digits=3, ...)
#' @S3method print analyze.data.frame
#' @param x a \code{analuze.data.frame} object
#' @param digits an integer specifying the precision of numeric variables
#' @param ... ignored parameters
#' @return the original object (invisibly)
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.analyze.data.frame <- function (x, digits=3, ...) {
  label <- attr(x, 'label')
  call <- attr(x, 'call')

  cat("Label:", label, "\n")
  cat("Call:  ")
  print(call)
  cat("\n")

  size <- length(x)

  for (column in names(x)) {
    cat("> Column name:", column, "\n")
    print(x[[column]], digits=digits)

    # Place a new-line between elements (but not after the last)
    if (size <- size - 1)
      cat("\n")
  }

  invisible(x)
}
