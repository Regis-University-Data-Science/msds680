library(data.table)

fn <- '/home/nate/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week1/heart.disease.data'

dt <- fread(fn)
summary(dt)
str(dt)
dt <- dt[, lapply(.SD, as.numeric)]
# replace all -9 with NA
dt[dt == -9] <- NA
summary(dt)
dim(dt)
# almost all 'dm' values are missing, so throw out that column
dt[, dm:=NULL]
# impute missing values
library(DMwR)
dt.nona <- knnImputation(dt)
summary(dt.nona)


# cholesterol and cigs appear to have large outliers
labels <- colnames(dt.nona)
for (i in seq(dim(dt)[2])) {
  col.data <- dt.nona[, get(labels[i])]
  nlevs <- nlevels(as.factor(col.data))
  if (nlevs <= 10) {
    barplot(table(col.data), main = NULL, xlab = labels[i])
    # axis(1, at=seq(nlevs), labels=levels(as.factor(col.data)))
  } else {
    hist(as.numeric(col.data), main = NULL, xlab = labels[i])
  }
  boxplot(col.data, main = labels[i])
}

library(ggplot2)
ggplot(stack(dt.nona), aes(x = ind, y = values)) +
  geom_boxplot() + scale_y_continuous(trans = 'log10')

# Outliers seem to be one low in thalach, a few high on cigs, and few high/low on cholestorol and trestbps.
# Looking carefully, it seems there are some lower cigs outliers we don't want to remove.  
# We removed the upper ones because 100 cigarettes a day seems like an error in the data.
max(dt.nona$cigs)
library(raster)
upper_outlier_cols <- c('trestbps', 'chol', 'cigs')
lower_outlier_cols <- c('trestbps', 'thalach', 'chol')
remove_outliers_up <- function(x) clamp(x, upper = boxplot.stats(x)$stats[5])
remove_outliers_low <- function(x) clamp(x, lower = boxplot.stats(x)$stats[1])

dt.nona[, upper_outlier_cols] <- dt.nona[, lapply(.SD, FUN = remove_outliers_up), .SDcols = upper_outlier_cols]
dt.nona[, lower_outlier_cols] <- dt.nona[, lapply(.SD, FUN = remove_outliers_low), .SDcols = lower_outlier_cols]
# you may have also noticed there is one weird 5.92... number in thal, this should be rounded to 6
dt.nona[, thal:=round(thal)]
# save data for later use
fn <- '~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week1/heart.disease.data.clean.csv'
fwrite(dt.nona, fn)


ggplot(stack(dt.nona), aes(x = ind, y = values)) +
  geom_boxplot() + scale_y_continuous(trans = 'log10')
# clip outliers -- first try with quantiles
# upper_90th <- apply(dt.nona, 2, function(x, prob=0.95) { quantile(x, prob, names=F) })
# quantile(dt.nona$cigs)  # could use this to get IQR, etc
# print(upper_90th)
# library(raster)
# dt.nona[, chol:=clamp(dt.nona$chol, upper = upper_90th[['chol']])]
# dt.nona[, cigs:=clamp(dt.nona$cigs, upper = upper_90th[['cigs']])]

# look at histograms again to make sure they look ok
for (i in seq(dim(dt)[2])) {
  col.data <- dt.nona[, get(labels[i])]
  nlevs <- nlevels(as.factor(col.data))
  if (nlevs <= 10) {
    barplot(table(col.data), main = NULL, xlab = labels[i])
    # axis(1, at=seq(nlevs), labels=levels(as.factor(col.data)))
  } else {
    hist(as.numeric(col.data), main = NULL, xlab = labels[i])
  }
  boxplot(col.data, main = labels[i])
}

# also boxplots


library(corrplot)
corrplot(cor(dt.nona), mar=c(3, 1, 1, 1))  # play around with margin parameters till it fits the screen
# we can see a number of things are positively correlated to the diagnosis, all except fasting blood sugar and cholesterol
# heart rate is the only thing negatively correlated, so the higher the heart rate (during exercise) the less chance of heart disease

