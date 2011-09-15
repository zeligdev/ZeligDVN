library(ZeligDVN)
data(turnout)
ZeligDVN:::info.matrix(cbind(turnout$race, turnout$vote))
