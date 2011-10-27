#' Analyze a Column of a data.frame
#'
#' Analyze a column of a data.frame and return important information relevant
#' to dataverse analysises. This is a necessary component for analyzing entire
#' data.frame's.
#' @param obj a data.frame
#' @param column a character-string specifying a column of data
#' @param threshold an integer specifying the maximum size of allowing numeric
#' data to be considered potential factor data. That is, if a data.frame
#' contains a numeric column with less unique elements than the value set by
#' 'threshold', then this method will consider it valid factor-style data.
#' @return an object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
AnalyzeColumn <- function (obj, column, threshold=100) {

  if (!is.character(column) && length(column) != 1) {
    warning("column is not a character-string")
    return(NULL)
  }

  if (!is.data.frame(obj)) {
    warning("obj is not a data.frame")
    return(NULL)
  }

  if (! column %in% names(obj)) {
    warning("column ", column, " is not in the specified data.frame")
    return(NULL)
  }

  values <- obj[[column]]

  if (is.atomic(values))
    info.atomic(values, threshold=threshold)

  else if (is.matrix(values))
    info.matrix(values, threshold=threshold)

  else
    NA
}
