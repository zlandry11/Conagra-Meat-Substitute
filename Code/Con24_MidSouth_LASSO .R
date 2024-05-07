#Let use a LASSO for MidSouth data
setwd("/Users/zach_landry/Library/Mobile Documents/com~apple~CloudDocs/UTD 2023-2024/Predictive Analytics/Project")
#---------- Central Location Measures-------------------------------------------
rm(list=ls()) #Clear up the Working Environment
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
msGardein <- gardein[gardein$msDum == 1,]


#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample_ms <- sample.split(msGardein$logSales, SplitRatio = 0.7)
train_ms  <- subset(msGardein, sample_ms == TRUE)
test_ms   <- subset(msGardein, sample_ms == FALSE)

#define response variable
test_y_ms <- test_ms$unit_sales
train_y_ms <- train_ms$unit_sales

#define matrix of predictor variables
x_test_ms <- model.matrix(unit_sales ~ price_per_unit + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
                       + ppuUnitRedOnly + poulDum + nonMeatDum + seaDum + filetDum + grndDum + meatballDum + nuggetDum + 
                         pattyDum + slicedDum + flvAsianDum + flvBfstDum + flvFishnDum + flvFreshnDum + flvGrainDum + flvItlnDum
                       + flvRglrDum + flvSpicyDum + flvTxMxDum + tosBeefDum + tosChkDum  + tosPrkDum + tosSfDum + tosSsgDUm
                       + tosUnkDum + y20 + y21 + y22 + y23 + price_per_unit:y20 + price_per_unit:y21 + price_per_unit:y22
                       + price_per_unit:y23   + price_per_unit:ppuDispOnly + price_per_unit:ppuFeatNDisp 
                       + price_per_unit:ppuFeatOnly + price_per_unit:ppuNoMerch + price_per_unit:ppuUnitRedOnly
                       + filetDum :flvRglrDum + filetDum :flvAsianDum 
                       + nuggetDum:flvRglrDum + nuggetDum:flvTxMxDum + nuggetDum:flvGrainDum + nuggetDum: flvSpicyDum + nuggetDum:flvAsianDum
                       + pattyDum:flvRglrDum + pattyDum:flvTxMxDum + pattyDum:flvItlnDum + pattyDum:flvSpicyDum + pattyDum:flvFreshnDum + 
                         pattyDum:flvBfstDum + slicedDum:flvItlnDum + slicedDum:flvRglrDum 
                       + poulDum:price_per_unit + nonMeatDum:price_per_unit + seaDum:price_per_unit + filetDum:price_per_unit
                       + grndDum:price_per_unit + meatballDum:price_per_unit + nuggetDum:price_per_unit + 
                         pattyDum:price_per_unit + slicedDum :price_per_unit, data = test_ms)


x_train_ms <- model.matrix(unit_sales ~ price_per_unit + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
                           + ppuUnitRedOnly + poulDum + nonMeatDum + seaDum + filetDum + grndDum + meatballDum + nuggetDum + 
                             pattyDum + slicedDum + flvAsianDum + flvBfstDum + flvFishnDum + flvFreshnDum + flvGrainDum + flvItlnDum
                           + flvRglrDum + flvSpicyDum + flvTxMxDum + tosBeefDum + tosChkDum  + tosPrkDum + tosSfDum + tosSsgDUm
                           + tosUnkDum + y20 + y21 + y22 + y23 + price_per_unit:y20 + price_per_unit:y21 + price_per_unit:y22
                           + price_per_unit:y23   + price_per_unit:ppuDispOnly + price_per_unit:ppuFeatNDisp 
                           + price_per_unit:ppuFeatOnly + price_per_unit:ppuNoMerch + price_per_unit:ppuUnitRedOnly
                           + filetDum :flvRglrDum + filetDum :flvAsianDum 
                           + nuggetDum:flvRglrDum + nuggetDum:flvTxMxDum + nuggetDum:flvGrainDum + nuggetDum: flvSpicyDum + nuggetDum:flvAsianDum
                           + pattyDum:flvRglrDum + pattyDum:flvTxMxDum + pattyDum:flvItlnDum + pattyDum:flvSpicyDum + pattyDum:flvFreshnDum + 
                             pattyDum:flvBfstDum + slicedDum:flvItlnDum + slicedDum:flvRglrDum 
                           + poulDum:price_per_unit + nonMeatDum:price_per_unit + seaDum:price_per_unit + filetDum:price_per_unit
                           + grndDum:price_per_unit + meatballDum:price_per_unit + nuggetDum:price_per_unit + 
                             pattyDum:price_per_unit + slicedDum :price_per_unit, data = train_ms)



#perform k-fold cross-validation to find optimal lambda value
cv_model_ms <- cv.glmnet(x_train_ms, train_y_ms, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda_ms <- cv_model_ms$lambda.min
best_lambda_ms

plot(cv_model_ms)
summary(cv_model_ms)

#MSE
mse.min.ms <-min(cv_model_ms$cvm)
mse.min.ms
#MSE for the LASSO is 1714861

best_model_ms <- glmnet(x_train_ms, train_y_ms, alpha = 1, lambda = best_lambda_ms)

tLL_ms <- best_model_ms$nulldev - deviance(best_model_ms)
k_ms <- best_model_ms$df
n_ms <- best_model_ms$nobs
AICc_ms <- -tLL_ms+2*k_ms+2*k_ms*(k_ms+1)/(n_ms-k_ms-1)
AICc_ms

coef(best_model_ms)

#use fitted best model to make predictions
y_predicted_ms <- predict(best_model_ms, s = best_lambda_ms, newx = x_test_ms)

plot(y_predicted_ms, test_y_ms, xlab="predicted", ylab="actual")
abline(a=0,b=1, col= 'red')

#find SST and SSE
sst_ms <- sum((test_y_ms - mean(test_y_ms))^2)
sse_ms <- sum((y_predicted_ms - test_y_ms)^2)

#find R-Squared
rsq_ms <- 1 - sse_ms/sst_ms
rsq_ms # R-squared for test data predictions is 0.7900707

adjR2_ms = 1-(((1-rsq_ms)*(n_ms))/(n_ms-k_ms-1))
adjR2_ms #0.7874406
