library(data.table)
filename <- '~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week1/data/heart.disease.data.clean.csv'
dt <- fread(filename)
str(dt)
dt[, thal:=as.factor(thal)]
dt[, num:=as.factor(num)]
dt[, ]


# min-max hill climbing
mmhc()