library(ZeligDVN)

# Univariate Outcome with Single Predictor Formula
explanatory <- c("race", "educate")
outcome <- "vote"

# 
write.script("logit", explanatory, outcome)
