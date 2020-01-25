rm(list = ls())

#setting the directory
setwd("E:/Analytics/edWisor/Project")

#importing the database
daydb  = read.csv("day.csv")

colnames(daydb)

##Analysis of Data
#Checking the data types
str(daydb)

#Count of unique values
table(daydb$season)
table(daydb$yr)
table(daydb$mnth)
table(daydb$holiday)
table(daydb$weekday)
table(daydb$workingday)
table(daydb$weathersit)

#Missing value analysis
miss_val = data.frame(apply(daydb, 2, function(i){sum(is.na(i))}))

#there are no null values in the data, so no need for imputing null values

library(ggplot2)

#distribution of categorical independant variables
par(mfrow=c(3,3))
barplot(table(daydb$yr),width = 0.5, main = "Year Dist.", xlab = "Year", ylab = "Freq.")
barplot(table(daydb$season),width = 0.5, main = "Season Dist.", xlab = "Season", ylab = "Freq.")
barplot(table(daydb$mnth),width = 0.5, main = "Month Dist.", xlab = "Month", ylab = "Freq.")
barplot(table(daydb$holiday),width = 0.5, main = "Holiday Dist.", xlab = "Holiday", ylab = "Freq.")
barplot(table(daydb$weekday),width = 0.5, main = "Weekday Dist.", xlab = "Weekday", ylab = "Freq.")
barplot(table(daydb$workingday),width = 0.5, main = "Working Day Dist.", xlab = "Working Day", ylab = "Freq.")
barplot(table(daydb$weathersit),width = 0.5, main = "Weather Dist.", xlab = "Weather", ylab = "Freq.")

#distribution of continuous independant variables
par(mfrow=c(2,2))
hist(daydb$temp)
hist(daydb$atemp)
hist(daydb$hum)
hist(daydb$windspeed)

# #distribution & outlier analysis of casual riders
par(mfrow=c(1,2))
hist(daydb$casual)
boxplot(daydb$casual)


# #distribution & outlier analysis of Registered riders
par(mfrow=c(1,2))
hist(daydb$registered)
boxplot(daydb$registered)


# #distribution & outlier analysis of Registered riders
par(mfrow=c(1,2))
hist(daydb$cnt)
boxplot(daydb$cnt)

#Checking of Multi-colinearity between variables

library(corrgram)
corrgram(daydb, order = F, main = "Correlation between variables")

#temp & atemp are highly positively correlated, and as they are independant variable, we can drop atemp.
#As cnt is the sum of registered and casual and we need to predict total bike rental count only, we can drop both casual & registered.
# Also season and month seems to be dependant, So we can remove month, as in our problem statement, seasonal settings is one factor.
#Also instant, dteday won't make any difference in predicting the dependant varialble, so removing them as well.

# Lets create a subset after removing all the variables except seasonal & environmental

bike_rental = subset(daydb, select = -c(instant, dteday, atemp, mnth, casual, registered))
for (i in (colnames(bike_rental))) {
  bike_rental[,which(colnames(bike_rental) == i)] = as.numeric(bike_rental[,which(colnames(bike_rental) == i)])
}

bike_rental$cntlog = log10(bike_rental$cnt)

bike_rental = subset(bike_rental, select = -c(cnt))

##Model Development
#Clean the environment
rm(list = setdiff(ls(), c("bike_rental", "daydb")))

# Divide data into test & train
train_index = sample(1:nrow(bike_rental), 0.8 * nrow(bike_rental))
train = bike_rental[train_index,]
test = bike_rental[-train_index,]

#Here we need to use the regression models
# Error Metrics
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}


# #Decision Tree Regression
# library(rpart)
# d_fit = rpart(cntlog ~ ., data = train, method = "anova")
# pred_new = predict(d_fit, test[,-(which(colnames(test) == "cntlog"))])
# 
# 
# 
# MAPE(test[,which(colnames(test) == "cntlog")], pred_new)

# Error Rate: 0.02219195
# Accuracy: 97.78%

# #Liner Regression Model
# install.packages("usdm")
# library(usdm)
# vif(bike_rental[,-10])
# 
# vifcor(bike_rental[,-10], th = 0.9)
# 
# 
# lm_model = lm(cntlog ~ ., data = train)
# 
# #Summary of the model
# summary(lm_model)
# 
# #Predict
# predictions_LR = predict(lm_model, test[,-(which(colnames(test) == "cntlog"))])
# 
# #Calculate MAPE
# MAPE(test[,10], predictions_LR)

# Error Rate: 0.02601011
# Accuracy: 97.39%

###Random Forest Model
library(randomForest)
RF_model = randomForest(cntlog ~ ., train, importance = TRUE, ntree = 100)
pred_RF = predict(RF_model, test[, -(which(colnames(test) == "cntlog"))])

MAPE(test[,which(colnames(test) == "cntlog")], pred_RF)

# Error Rate: 0.01712123
# Accuracy: 98.28%

# #Logistic Regression
# logit_model = glm(cntlog ~ ., data = train, family = "quasi")
# 
# #summary of the model
# summary(logit_model)
# 
# #predict using logistic regression
# pred_logit = predict(logit_model, test[, -(which(colnames(test) == "cntlog"))])
# 
# MAPE(test[,10], pred_logit)
# 
# #Error Rate: 0.1604 (gaussian & quasi)
# #Accuracy: 85.96% (gaussian & quasi)
# 
# # With all variables numeric & cnt log
# # Error Rate: 0.02601011
# # Accuracy: 97.39%

# Reading Sample Data from CSV and fitting with the model to get the output
sam_db = read.csv("Sample-data.csv")
str(sam_db)
sam_data = subset(sam_db, select = -c(instant, dteday, atemp, mnth))
sam_count = predict(RF_model, sam_data)
sam_db$cnt = round(10^sam_count)
# I/P- instant:1000, dteday:10-05-2014, season:2, yr:4, mnth:5, holiday:0, weekday:6, workingday:0, 
# weathersit:1, temp:0.5325, atemp:0.522721, hum:0.489167, windspeed: 0.115671
# O/P: cnt = 7318

write.csv(sam_db, file = "Sample-data-op.csv")