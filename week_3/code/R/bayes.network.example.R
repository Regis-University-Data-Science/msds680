# adapted from here: https://www.r-bloggers.com/bayesian-network-in-r-introduction/
library(bnlearn)
library(data.table)
data(coronary)
bn.dt <- as.data.table(coronary)
head(bn.dt)
str(bn.dt)
bn.dt[`M. Work` == "yes" & `P. Work` == "yes"]
model <- hc(bn.dt)
plot(model)
# slightly better model generation algorithm
model <- tabu(bn.dt)
plot(model)
model
# if a connection doesn't make sense, we can remove it like this
model <- drop.arc(model, from = 'M. Work', to = 'Family')
plot(model)
# TODO lookup how to append matrix
# look for how I was converting multiple columns to numeric/factor at once
# add arc to model
model <- set.arc(model, from = 'Family', to = 'Proteins')
plot(model)

fittedbn <- bn.fit(model, data = bn.dt)
fittedbn

# how we can get a prediction from the model
cpquery(fittedbn, event = (Smoking=="no"), evidence = (`M. Work`=="no") )
cpquery(fittedbn, event = (Smoking=="no"), evidence = (`M. Work`=="yes") )
cpquery(fittedbn, event = (Smoking=="yes"), evidence = (`M. Work`=="yes") )
cpquery(fittedbn, event = (Smoking=="yes"), evidence = (`M. Work`=="yes" & `P. Work`=="yes") )
cpquery(fittedbn, event = (Smoking=="yes"), evidence = (`P. Work`=="yes") )
