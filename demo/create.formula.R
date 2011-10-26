# ...

library(ZeligDVN)

# Univariate Outcome with Single Predictor Formula
explanatory <- c("race", "educate")
outcome <- "vote"
create.formula(outcome, explanatory)

# Multivariate Outcome with Single Predictor Formula
explanatory <- c("race", "year")
outcome <- list("vote", "income")
create.formula(outcome, explanatory)

# Multivariate Outcome with Multiple Predictor Formulae
explanatory <- list("race", c("educate", "race"))
outcome <- c("vote", "income")
create.formula(outcome, explanatory)
