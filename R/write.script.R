#' Write Script for DVN
#'
#' This function creates a script (as a string) to compute a statistical model
#' for use with the Dataverse.
#' @param model a character-string specifying the model to compute
#' @param outcome a character-vector specifying outcome variables
#' @param explanatory a character-vector or list of character-vectors
#' specifying explanatory variables for each outcome variable
#' @param intercept the intercept parameter - setting this value to 0 will 
#' fit the statistical model without using an intercept
#' @return nothing - this function is used for its spillover
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
write.script <- function (model, outcome, explanatory, intercept = 1, con = stdout()) {
  form <- create.formula(outcome, explanatory, intercept)
  formula.to.string <- function (x) { as.character(as.expression(x)) }

  form <- if (is.list(form))
    Map(formula.to.string, form)
  else
    formula.to.string(form)

  form
}
