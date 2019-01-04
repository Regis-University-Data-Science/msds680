library(data.table)
set.seed(42)  # set the random seed for reproducibility

filename <- '~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week1/data/heart.disease.data.clean.csv'
heart.dt <- fread(filename)
str(heart.dt)
col.labels <- c('age',
                'sex',
                'chest.pain.type',
                'resting.blood.pressure',
                'cholesterol',
                'num.cigs.per.day',
                'years.as.smoker',
                'fasting.blood.sugar',
                'family.history.heart.disease',
                'resting.ecg.results',
                'max.heart.rate.exercise',
                'exercise.induced.angina',
                'blood.disorder',  # Thalassemia I think
                'heart.disease')

names(heart.dt) <- col.labels
# convert heart disease to binary outcome, 1 is heart disease
heart.dt[heart.disease > 0, heart.disease:=1]
heart.dt[, heart.disease:=as.factor(heart.disease)]
dim(heart.dt)  # 13 features -- we will need this for a hyperparameter in a sec

library(caret)
tr.idxs <- createDataPartition(heart.dt$heart.disease, p = 0.8)$Resample1
train <- heart.dt[tr.idxs]
test <- heart.dt[-tr.idxs]

# enable parallel processing with all except for one of our CPU cores
library(parallel)
library(doMC)
registerDoMC(cores = detectCores() - 1)

trControl <- trainControl(method = 'repeatedcv',
                          number = 3,
                          repeats = 4)

rf.model <- train(heart.disease ~ .,
                  data = train,
                  method = 'rf',
                  trControl = trControl,
                  ntree = 250,  # makes it run a bit faster
                  nodesize = 15,  # minimum number of samples in a leaf node
                  maxnodes = 5,  # max number of leaf nodes in each tree
                  tuneGrid = expand.grid(mtry = c(4, 8, 12)))

rf.model

tr.preds <- predict(rf.model, train)
postResample(tr.preds, train$heart.disease)

te.preds <- predict(rf.model, test)
postResample(te.preds, test$heart.disease)

varImp(rf.model)
library(randomForest)
varImpPlot(rf.model$finalModel, scale = F)
