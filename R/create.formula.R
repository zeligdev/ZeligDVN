#' Create a Formula from Inputs
#'
#' ...
#' @param outcome
#' @param explanatory
#' @return a formula
#' @export
create.formula <- function (outcome, explanatory, intercept = 1, envir=parent.frame()) {
  if (length(outcome) < 1)
    NULL

  else if (length(outcome) == 1)
    univariate.formula(outcome, explanatory, intercept, envir=envir)

  else if (!is.list(explanatory))
    multivariate.formula(outcome, explanatory, intercept, envir=envir)

  else
    multivariate.list.formula(outcome, explanatory, intercept, envir=envir)
}

#' Create a Formula with a Univariate Outcome
#'
#' ...
#' @param outcome a character-string specifying the outcome variables
#' @param explanatory a character-vector specifying the explanatory variables
#' @param intercept an integer specifying whether an intercept exists
#' @return a formula with a single outcome variable
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
univariate.formula <- function (outcome, explanatory, intercept, envir) {
  if (length(outcome) != 1) {
    warning("...")
    return(~ 1)
  }

  rhs <- paste(unique(explanatory), collapse = " + ")

  if (intercept == 0)
    rhs <- paste(rhs, 0, sep = " + ")

  form <- paste(outcome, rhs, sep = " ~ ")

  as.formula(form, envir)
}

#' Create a Formula with a Multivariate Outcome and *System* of Explanatory Variables
#'
#' ...
#' @param outcome a character-vector specifying the outcome variables
#' @param explanatory a character-vector specifying the explanatory variables
#' @param intercept an integer specifying whether an intercept exists
#' @return a formula with a single outcome variable
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
multivariate.formula <- function (outcome, explanatory, intercept, as.list=FALSE, envir) {

  if (as.list)
    multivariate.list.formula(outcome, explanatory, intercept, envir)

  lhs <- paste(outcome, collapse = ", ")
  lhs <- paste("cbind(", lhs, ")", sep="")
  rhs <- paste(unique(explanatory), collapse = " + ")

  if (intercept == 0)
    rhs <- paste(rhs, 0, sep = " + ")

  as.formula(paste(lhs, "~", rhs), env=envir)
}

#' Create a Formula with a Multivariate Outcome and *System* of Explanatory Variables
#'
#' ...
#' @param outcome a character-vector specifying the outcome variables
#' @param explanatory a character-vector specifying the explanatory variables
#' @param intercept an integer specifying whether an intercept exists
#' @return a formula with a single outcome variable
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
multivariate.list.formula <- function (outcome, explanatory, intercept, envir) {

  if (length(outcome) > length(explanatory))
    warning("There are more outcome variables listed than explanatory equations. ",
            "Ignoring extra outcome variables.")

  # Result list
  results <- list()

  # Iterate through explanatory variables
  for (k in 1:length(explanatory)) {

    rhs <- explanatory[[k]]

    # If there are no outcome variables, then simply omit them from the forumla
    lhs <- if (k > length(outcome))
      ""

    # Otherwise, business as usual
    else
      outcome[k]

    # Merge lefthand side and righthand side as if they form a 
    # univariate-outcome formula
    results[[k]] <- univariate.formula(lhs, rhs, intercept, envir)
  }

  results
}
