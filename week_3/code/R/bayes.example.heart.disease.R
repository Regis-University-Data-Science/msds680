# https://topepo.github.io/caret/train-models-by-tag.html#Bayesian_Model
# search for 'bayes' to find models using Bayes' Law
library(data.table)
set.seed(42)
filename <- '~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week1/data/heart.disease.data.clean.csv'
dt <- fread(filename)
str(dt)
dt[num > 0, num:=1]
dt[, num:=as.factor(num)]
str(dt)

library(caret)

train.indices <- createDataPartition(dt$num, p = 0.8)$Resample1
train <- dt[train.indices]
test <- dt[-train.indices]

trControl <- trainControl(method  = "repeatedcv",
                          number  = 3,
                          repeats = 2)

# how to see what parameters are available for a model:
# search for model here
# https://topepo.github.io/caret/available-models.html
# then look up documentation for function from that package on rdocumentation.org

fit <- train(num ~ .,
             method     = 'nb',  # naive bayes from klaR package
             tuneGrid   = data.frame("fL"=0, "usekernel"=c(T, F), "adjust"=1),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = train)

fit

# get scores from predictions
postResample(predict(fit, test), test$num)

confusionMatrix(predict(fit, train), train$num)
