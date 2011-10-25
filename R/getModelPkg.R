#' Get Package Name of a Zelig Model
#'
#' @param model a character-string specifying a Zelig model
#' @return a character-string specifying the pacakge or NA (if the model does
#' not exist)
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
getModelPkg <- function (model) {
  models <- Zelig:::list.zelig.models()
  as.character(models[model])
}
