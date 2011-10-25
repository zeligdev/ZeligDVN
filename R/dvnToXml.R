#' Convert \code{dvn} File to \code{xml}
#'
#' Converts DVN-style documents to an XML equivalent.
#' @note This function is used 
#' @param model a character-string specifying the model
#' @param dvn.file a character-string specifying the location of a dvn-file
#' @param con a connection to output
#' @return nothing 
#' @export
dvnToXml <- function (model, dvn.file, con=stdout()) {
  descr <- dvn.description(model, dvn.file)
  writeLines("<?XML>", con)
  writeLines("<!doctpe XML>", con)
  writeLines("<model>", con)
  writeLines(make.description(descr), con)
  writeLines(make.helpLink(descr), con)
  writeLines(make.packageDependency(descr), con)
  writeLines(make.formula(descr), con)
  writeLines('  <setx maxSetx="2" />', con)
  writeLines("</model>", con)

  invisible()
}

