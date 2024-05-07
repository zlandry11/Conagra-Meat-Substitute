#Let use a LASSO for the California data
setwd("/Users/saipyneni/Documents/Conagra Project/Conagra Project/Data")
#---------- Central Location Measures-------------------------------------------
rm(list=ls()) #Clear up the Working Environment
options(scipen=999)
library(dplyr)
library(readxl)
library(pastecs)
install.packages("DescTools")                # Install DescTools package
library("DescTools")    #importthe data
library(ggplot2)
library(ggrepel)
library(stringr)
library(tidyr)
library(forcats)
install.packages("stargazer")               
library(stargazer)

#import Dataset to work
install.packages('glmnet')
library(glmnet)
#splitting the data
library(caTools)
# used for plotting 
library(ggplot2) 
# used for modeling
library(caret) 
# used for building XGBoost model
library(xgboost)  
# used for skewness
library(e1071)  
# used for combining multiple plots 
library(cowplot) 
install.packages("caret")
library(caret)

gardein <- read.csv("gardeinCleaned.csv")
calGardein <- gardein[gardein$calDum == 1,]

#calGardein$logSales <- log(calGardein$unit_sales)
#calGardein$logUnitPrice <- log(calGardein$price_per_unit)


#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample_cal <- sample.split(calGardein$logSales, SplitRatio = 0.7)
train_cal  <- subset(calGardein, sample_cal == TRUE)
test_cal   <- subset(calGardein, sample_cal == FALSE)

#define response variable
test_y_cal <- test_cal$unit_sales
train_y_cal <- train_cal$unit_sales

#define matrix of predictor variables
x_test_cal <- model.matrix(unit_sales ~ price_per_unit + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
                       + ppuUnitRedOnly + poulDum + nonMeatDum + seaDum + filetDum + grndDum + meatballDum + nuggetDum + 
                         pattyDum + slicedDum + flvAsianDum + flvBfstDum + flvFishnDum + flvFreshnDum + flvGrainDum + flvItlnDum
                       + flvRglrDum + flvSpicyDum + flvTxMxDum + tosBeefDum + tosChkDum  + tosPrkDum + tosSfDum + tosSsgDUm
                       + tosUnkDum + y20 + y21 + y22 + y23 + price_per_unit:y20 + price_per_unit:y21 + price_per_unit:y22
                       + price_per_unit:y23  + price_per_unit:ppuDispOnly + price_per_unit:ppuFeatNDisp 
                       + price_per_unit:ppuFeatOnly + price_per_unit:ppuNoMerch + price_per_unit:ppuUnitRedOnly
                       + filetDum :flvRglrDum + filetDum :flvAsianDum 
                       + nuggetDum:flvRglrDum + nuggetDum:flvTxMxDum + nuggetDum:flvGrainDum + nuggetDum: flvSpicyDum + nuggetDum:flvAsianDum
                       + pattyDum:flvRglrDum + pattyDum:flvTxMxDum + pattyDum:flvItlnDum + pattyDum:flvSpicyDum + pattyDum:flvFreshnDum + 
                         pattyDum:flvBfstDum + slicedDum:flvItlnDum + slicedDum:flvRglrDum 
                       + poulDum:price_per_unit + nonMeatDum:price_per_unit + seaDum:price_per_unit + filetDum:price_per_unit
                       + grndDum:price_per_unit + meatballDum:price_per_unit + nuggetDum:price_per_unit + 
                         pattyDum:price_per_unit + slicedDum :price_per_unit, data = test_cal)


x_train_cal <- model.matrix(unit_sales ~ price_per_unit + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
                            + ppuUnitRedOnly + poulDum + nonMeatDum + seaDum + filetDum + grndDum + meatballDum + nuggetDum + 
                              pattyDum + slicedDum + flvAsianDum + flvBfstDum + flvFishnDum + flvFreshnDum + flvGrainDum + flvItlnDum
                            + flvRglrDum + flvSpicyDum + flvTxMxDum + tosBeefDum + tosChkDum  + tosPrkDum + tosSfDum + tosSsgDUm
                            + tosUnkDum + y20 + y21 + y22 + y23 + price_per_unit:y20 + price_per_unit:y21 + price_per_unit:y22
                            + price_per_unit:y23  + price_per_unit:ppuDispOnly + price_per_unit:ppuFeatNDisp 
                            + price_per_unit:ppuFeatOnly + price_per_unit:ppuNoMerch + price_per_unit:ppuUnitRedOnly
                            + filetDum :flvRglrDum + filetDum :flvAsianDum 
                            + nuggetDum:flvRglrDum + nuggetDum:flvTxMxDum + nuggetDum:flvGrainDum + nuggetDum: flvSpicyDum + nuggetDum:flvAsianDum
                            + pattyDum:flvRglrDum + pattyDum:flvTxMxDum + pattyDum:flvItlnDum + pattyDum:flvSpicyDum + pattyDum:flvFreshnDum + 
                              pattyDum:flvBfstDum + slicedDum:flvItlnDum + slicedDum:flvRglrDum 
                            + poulDum:price_per_unit + nonMeatDum:price_per_unit + seaDum:price_per_unit + filetDum:price_per_unit
                            + grndDum:price_per_unit + meatballDum:price_per_unit + nuggetDum:price_per_unit + 
                              pattyDum:price_per_unit + slicedDum :price_per_unit, = train_cal)



#perform k-fold cross-validation to find optimal lambda value
cv_model_cal <- cv.glmnet(x_train_cal, train_y_cal, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda_cal <- cv_model_cal$lambda.min
best_lambda_cal

plot(cv_model_cal)
summary(cv_model_cal)

#MSE
mse.min.cal <-min(cv_model_cal$cvm)
mse.min.cal
#MSE for the LASSO is 1664560


best_model_cal <- glmnet(x_test_cal, test_y_cal, alpha = 1, lambda = best_lambda_cal)
coef(best_model_cal)


tLL_cal <- best_model_cal$nulldev - deviance(best_model_cal)
k_cal <- best_model_cal$df
n_cal <- best_model_cal$nobs
AICc_cal <- -tLL_cal+2*k_cal+2*k_cal*(k_cal+1)/(n_cal-k_cal-1)
AICc_cal

#use fitted best model to make predictions
y_predicted_cal <- predict(best_model_cal, s = best_lambda_cal, newx = x_train_cal)


plot(y_predicted_cal,train_y_cal,
     xlab="predicted",ylab="actual")
abline(a=0,b=1, col= 'red')

#find SST and SSE
sst_cal <- sum((train_y_cal - mean(train_y_cal))^2)
sse_cal <- sum((y_predicted_cal - train_y_cal)^2)

#find R-Squared, with 52 0.6
rsq_cal <- 1 - sse_cal/sst_cal
rsq_cal


#adjR2
adjR2_cal = 1-(((1-rsq_cal)*(n_cal-1))/(n_cal-k_cal-1))
adjR2_cal
