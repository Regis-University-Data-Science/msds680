library(data.table)
library(caret)
set.seed(42)
iris.dt <- as.data.table(iris)

train.idxs <- createDataPartition(iris.dt$Species, p = 0.8)$Resample1
train <- iris.dt[train.idxs]
test <- iris.dt[-train.idxs]

library(rpart)
model <- rpart(Species ~ ., data = train, method = 'class', control = list(minsplit = 1))
plot(model)
text(model)
pred.matrix <- predict(model, test)
# gets max across rows
preds <- colnames(pred.matrix)[apply(pred.matrix, 1, which.max)]
confusionMatrix(preds, test$Species)
# works perfectly, but too easy of an example


filename <- '~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week1/data/auto.dt.nona.csv'
mpg.dt <- fread(filename)
train.idxs <- createDataPartition(mpg.dt$mpg, p = 0.8)$Resample1
train <- mpg.dt[train.idxs]
test <- mpg.dt[-train.idxs]

# we are setting some hyperparameters to be rediculous so we will overfit on purpose
model <- rpart(mpg ~ ., data = train, method = 'anova', control = list(minsplit = 1, cp = 0.00001))
plot(model)
text(model)
preds <- predict(model, test)
postResample(preds, test$mpg)
preds <- predict(model, train)
postResample(preds, train$mpg)
# test scoring much lower than train, sign of overitting
summary(model)
# still don't have any leaf nodes with 1 sample -- that would be the most extreme overfitting