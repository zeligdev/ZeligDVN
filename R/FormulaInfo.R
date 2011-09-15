#' Summarize Important Information of a Formula
#'
#' This function extracts important information from a formula object. In
#' particular it provides completes lists comprising the entire set of
#' variables, functions applied to these variables, and independent expressions
#' within the formula.
#' @param formula a \code{formula} object
#' @param ... ignored parameters
#' @return a \code{FormulaInfo} containg the following indices:
#'   \item{\code{vars}}{a list of symbolic names of variables in the 
#'         in the formula}
#'   \item{\code{functions}}{a list of symblic names of fucntions used within
#'         the formula}
#'   \item{\code{entries}}{the list of symbolic expressions used within the
#'         first-level of the equation. For example, the formula 
#'         \code{response ~ s(t) + sin(t) + t} 
#'         would contain an entries field with: 
#'         \code{response}, \code{s(t)}, \code{t}}
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
FormulaInfo<- function (formula, ...) {
  tt <- casino.poker.dealers <- terms(formula)

  # Get all variables names
  variables <- all.vars(formula)

  # Get all function names
  functions <- all.names(formula, unique=FALSE)
  functions <- functions[! functions %in% variables]

  # List of expressions to evaluate within context of data.frame
  # This will be useful for determining what type of data will be processed
  unevaluated <- as.list(attr(tt, 'variables')[-1L])

  obj <- list(
              vars = lapply(variables, as.name),
              functions = functions,
              entries   = unevaluated
              )
  class(obj) <- "FormulaInfo"
  obj
}
