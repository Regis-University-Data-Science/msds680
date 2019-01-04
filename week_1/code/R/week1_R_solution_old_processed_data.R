library(data.table)

fn <- '/home/nate/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week1/processed.cleveland.data'

dt <- fread(fn)
summary(dt)
str(dt)

# set the labels
labels <- c('age',
            'gender',  # 1 = male, 0 = female
            'chest_pain_type',  # cp type ranges from 1-4, 1 seems worst, 4 seems best
            'resting_bp',
            'cholesterol_lvl',
            'fasting_blood_sugar',
            'resting_ecg_result',  # 0 seems best, 2 seems worst
            'max_heart_rate',  # I think during exercise
            'exercise_induced_angina',  # 1 = yes, 0 = no
            'oldpeak',  # something about ECG
            'slope',  # another ECG thing
            'num_major_vessels',
            'thal',  # 3 = normal; 6 = fixed defect; 7 = reversable defect 
            'diagnosis')  # this is the target variable, 0 = no heart diseas, 1-4 = increasing heart disease

colnames(dt) <- labels
str(dt)
# convert to numeric so we can look at correlations
dt[, num_major_vessels:=as.numeric(num_major_vessels)]
dt[, thal:=as.numeric(thal)]
str(dt)
summary(dt)
for (i in seq(dim(dt)[2])) {
  print(labels[i])
  hist(dt[, get(labels[i])], main = NULL, xlab = labels[i])
}

library(corrplot)
corrplot(cor(dt), mar=c(3, 1, 1, 1))  # play around with margin parameters till it fits the screen
# we can see a number of things are positively correlated to the diagnosis, all except fasting blood sugar and cholesterol
# heart rate is the only thing negatively correlated, so the higher the heart rate (during exercise) the less chance of heart disease


