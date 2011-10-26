# Make various XML Structures for use with DVN


#' Make Description
make.description <- function (descr) {
  strpad(sprintf("<description>%s</description>", descr$description), 2)
}

#' Make Help Link
make.helpLink <- function (descr) {
  url <- "http://gking.harvard.edu"
  strpad(sprintf('<helpLink url="%s" />', descr$help.url), 2)
}

#' Make Packaged Dependencies
make.packageDependency <- function (descr) {
  tags <- NULL
  form <- '<packageDependency name="%s" version="%s" />'

  for (dep in descr$depends) {
    pkg.info <- split.pkg(descr$depends)
    tag <- sprintf(form, pkg.info["name"], pkg.info["comment"])
    tags <- c(tags, tag)
  }

  strpad(tags, 2)
}

#' Make Formula
make.formula <- function (descr) {
  outcome <- strpad(make.outcome(descr), 2)
  explanatory <- strpad(make.explanatory(descr), 2)
  lines <- c(
             '<equation minEquations="">',
             outcome,
             explanatory,
             '</equation>'
             )
  strpad(lines, 2)
}

#' Make Outcome Section (of Formula)
make.outcome <- function (descr) {
  "<outcome><!-- OUTCOME VARIABLES --></outcome>"
}

#' Make Explantory Section (of Formula)
make.explanatory <- function (descr) {
  tags <- NULL

  for (type in descr$explanatory) {
    tag <- sprintf("<modelingType>%s</modelingType>", type)
    tags <- c(tags, strpad(tag, 2))
  }

  c(
    '<explanatory minVar="1">',
    tags,
    "</explanatory>"
    )
}
