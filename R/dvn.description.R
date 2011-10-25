#' Describe a DVN Relationship
#'
#' ...
#' @param model model name
#' @param dvn.file file
#' @return a dvn.description
#' @export
dvn.description <- function (model, dvn.file) {

  obj <- list(
              model = "logit",
              year  = 2011,
              help.url = "http://gking.harvard.edu/zelig/",
              url = "http://gking.harvard.edu/zelig/",
              depends = c("Survey (>= 1.0)"),
              description = "Exponential Regression for Duration Dependent Variables",
              explanatory = c("continuous", "discrete", "nominal", "ordinal", "binary"),
              outcome = "binary",
              min = 1,
              max = 1
              )
  class(obj) <- "dvn.description"
  obj
}

#' Print DVN Descriptions (as XML)
#' 
#' ,,,
#' @param x a dvn.description object
#' @param ... ignored parameters
#' @return x
#' @S3method print dvn.description
print.dvn.description <- function (x, ...) {
  dvnToXml(x)
  invisible(x)
}
