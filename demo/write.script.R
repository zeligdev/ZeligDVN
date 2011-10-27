library(ZeligDVN)

# Univariate Outcome with Single Predictor Formula
explanatory <- list("race", c("race", "educate"))
outcome <- c("vote", "income")

# 
message(write.script("logit", outcome, explanatory))
