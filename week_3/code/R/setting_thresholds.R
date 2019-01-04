filename <- '~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week1/data/heart.disease.data.clean.csv'

dt <- fread(filename)
str(dt)
hist(dt$age)
thresh <- median(dt$age)
dim(dt[age > thresh])
dim(dt[age < thresh])
dt[age > thresh, old:=T]
dt[age <= thresh, old:=F]
head(dt)
dt[, old:=as.factor(old)]
head(dt)
str(dt)
dt[, age:=NULL]
