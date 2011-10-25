#' Split Package Version String into Component Parts
#' 
#' This function splits a character-string specifying package dependency into 
#' its three parts: package name, version number and operator specifying
#' whether its dependency is an inequality.
#' @note This function is used internally by Zelig.
#' @param string a character-string formatted like a dependency requirement.
#' E.g., "Package (>= 3.0)", "Package", "Package (1.0)"
#' @return a character-vector with three names components: name, operator and
#' number, respectively corresponding to package name, equality operator and 
#' version number (exact or based on operator).
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
split.pkg <- function (string) {
  regex.name <- "^ *([a-zA-Z][a-zA-Z0-9\\.]*).*$"
  regex.comment <- ".*\\( *(.*) *\\)$ *"
  regex.number <- "^[^0-9\\.]*?([0-9]+(?:\\.|-)[0-9]+(?:(?:\\.|-)[0-9]+)?) *?$"
  regex.op <- " *(>=|<=).*"

  pkg.name <- gsub(regex.name, "\\1", string)
  pkg.comment <- gsub(regex.comment, "\\1", string)
  pkg.number <- gsub(regex.number, "\\1", pkg.comment)
  pkg.operator <- gsub(regex.op, "\\1", pkg.comment)

  if (!grepl("^[a-zA-Z][a-zA-Z0-9\\.]*$", pkg.name))
    pkg.name <- NA

  if (!grepl("^(>=|<=)$", pkg.operator))
    pkg.operator <- NA

  if (!grepl("^[0-9]+(?:\\.|-)[0-9]+((?:\\.|-)[0-9]+)?$", pkg.number)) {
    pkg.number <- NA
    pkg.operator <- NA
  }

  pkg.comment <- if (is.na(pkg.number))
    NA
  else if (is.na(pkg.operator))
    pkg.number
  else
    paste(pkg.operator, pkg.number)

  c(
    name = pkg.name,
    operator = pkg.operator,
    number = pkg.number,
    comment = pkg.comment
    )
}
