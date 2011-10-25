library(ZeligDVN)

isLegalPkg <- function (pkg) {
  grepl("^[a-zA-Z][a-zA-Z0-9\\.]*$", pkg)
}


model <- "logit"
pkg <- getModelPkg(model)

load.script <- ""

if (isLegalPkg(pkg))
  load.script <- sprintf("library(%s)", pkg)

load.script
