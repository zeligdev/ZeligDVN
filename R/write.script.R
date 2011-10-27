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


  # Create valid
  form <- if (is.list(form)) {
    form <- Map(formula.to.string, form)
    form <- paste(form, collapse=", ")
    sprintf("list(%s)", form)
  }

  else
    formula.to.string(form)

  # Generate Timestamped Title for Data Set
  timestamp <- format(Sys.time(), "%Y%j_%H%M%S.Rdata")
  data.frame.title <- "DataFile"
  stamped.data.set <- paste(data.frame.title, timestamp, sep="_")

  # Generate line that loads data frame
  data.line <- sprintf("load(\"%s.Rdata\")", stamped.data.set)

  # Generate line that fits statistical model
  zelig.line <- paste(
                      "z.out <- zelig(", form,
                      ", model=\"", model, "\"", 
                      ", data=", stamped.data.set, ")",
                      sep=""
                      )

  # Generate setx line 
  setx.line <- paste("x.out <- setx(z.out)")

  # Generate sim line 
  sim.line <- paste("s.out <- sim(x.out, z.out)")

  
  # Return a block of text.
  sprintf("%s
%s
%s
%s
plot(%s)
plot(%s)
", data.line, zelig.line, setx.line, sim.line, "z.out", "s.out")
}
