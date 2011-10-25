#' Pad Strings
#'
#' @param x a character vector
#' @param padding integer
#' @param char a character vector
#' @return a character vector
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
strpad <- function (x, padding, char=" ") {
  x <- unlist(strsplit(x, "\n"))
  padded.text <- paste(rep(char, padding), collapse="", sep="")
  paste(padded.text, x, sep="", collapse="\n")
}
