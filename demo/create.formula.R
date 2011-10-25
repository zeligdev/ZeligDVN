# ...

library(ZeligDVN)

explanatory <- c()
outcome <- c()

# outcome ~ explanatory
univariate.formula <- function (outcome, explanatory) {
  update(y ~ x, paste(outcome, "~", paste(explanatory, collapse=" + ")))
}

# cbind(outcome1, outcome2) ~ explantory
# list(outcome1, outcome2) ~ explanatory
multivariate.repeated.formula <- function (outcome, explanatory) {
}
