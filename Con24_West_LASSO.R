#Let use a LASSO for the West data
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
westGardein <- gardein[gardein$westDum == 1,]


#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample_west <- sample.split(westGardein$unit_sales, SplitRatio = 0.7)
train_west  <- subset(westGardein, sample_west == TRUE)
test_west   <- subset(westGardein, sample_west == FALSE)

#define response variable
test_y_west <- test_west$unit_sales
train_y_west <- train_west$unit_sales

#define matrix of predictor variables
x_test_west <- model.matrix(unit_sales ~ price_per_unit + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
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
                               pattyDum:price_per_unit + slicedDum :price_per_unit
                             , data = test_west)


x_train_west <- model.matrix(unit_sales ~ price_per_unit + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
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
                               pattyDum:price_per_unit + slicedDum :price_per_unit, data = train_west)



#perform k-fold cross-validation to find optimal lambda value
cv_model_west <- cv.glmnet(x_train_west, train_y_west, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda_west <- cv_model_west$lambda.min
best_lambda_west

plot(cv_model_west)
summary(cv_model_west)

#MSE
mse.min.west <-min(cv_model_west$cvm)
mse.min.west
#MSE for the LASSO is 1214057

best_model_west <- glmnet(x_train_west, train_y_west, alpha = 1, lambda = best_lambda_west)

tLL_west <- best_model_west$nulldev - deviance(best_model_west)
k_west <- best_model_west$df
n_west <- best_model_west$nobs
AICc_west <- -tLL_west+2*k_west+2*k_west*(k_west+1)/(n_west-k_west-1)
AICc_west

coef(best_model_west)

#use fitted best model to make predictions
y_predicted_west <- predict(best_model_west, s = best_lambda_west, newx = x_test_west)

plot(y_predicted_west, test_y_west, xlab="predicted", ylab="actual")
abline(a=0,b=1, col= 'red')

#find SST and SSE
sst_west <- sum((test_y_west - mean(test_y_west))^2)
sse_west <- sum((y_predicted_west - test_y_west)^2)

#find R-Squared
rsq_west <- 1 - sse_west/sst_west
rsq_west # R-squared for test data predictions

adjR2_west = 1-(((1-rsq_west)*(n_west-1))/(n_west-k_west-1))
adjR2_west
