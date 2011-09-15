library(ZeligDVN)
data(turnout)


form <- list(
             vote ~ income,
             vote ~ race + income
             )

analyze(form, turnout)

