pred.proba <- c(0.15, 0.23, 0.45, 0.55, 0.67, 0.88, 0.97)
actual <- c(0, 1, 0, 1, 0, 1, 1)

library(pROC)
roc.curve <- roc(actual, pred.proba)
roc.curve
plot(roc.curve)
roc.curve$auc

# gets the best point ahe ROC curve based on the sum of sensitivity and specificity
coords(roc = roc.curve, x = 'best', ret = 'threshold')

library(data.table)
dt <- data.table(cbind(pred.proba, actual))
names(dt) <- c('pred.proba', 'actual')
dt[, pred:=1]
dt[pred.proba < 0.775, pred:=0]
library(caret)
# careful!  The positive class is auto-detected as 0...
confusionMatrix(dt$pred, dt$actual)#, positive = '1')
dt[, pred:=as.factor(pred)]
confusionMatrix(as.factor(dt$pred), as.factor(dt$actual))
