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
totGardein <- gardein[gardein$totDum == 1,]


#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample_tot <- sample.split(totGardein$logSales, SplitRatio = 0.7)
train_tot  <- subset(totGardein, sample_tot == TRUE)
test_tot   <- subset(totGardein, sample_tot == FALSE)

#define response variable
test_y_tot <- test_tot$unit_sales
train_y_tot <- train_tot$unit_sales

#define matrix of predictor variables
x_test_tot <- model.matrix(unit_sales ~ price_per_unit + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
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
                             pattyDum:price_per_unit + slicedDum :price_per_unit, data = test_tot)


x_train_tot <- model.matrix(unit_sales ~ price_per_unit + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
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
                              pattyDum:price_per_unit + slicedDum :price_per_unit, data = train_tot)



#perform k-fold cross-validation to find optimal lambda value
cv_model_tot <- cv.glmnet(x_train_tot, train_y_tot, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda_tot <- cv_model_tot$lambda.min
best_lambda_tot

plot(cv_model_tot)
summary(cv_model_tot)

#MSE
mse.min.tot <-min(cv_model_tot$cvm)
mse.min.tot
#MSE for the LASSO is 1.373

best_model_tot <- glmnet(x_train_tot, train_y_tot, alpha = 1, lambda = best_lambda_tot)

tLL_tot <- best_model_tot$nulldev - deviance(best_model_tot)
k_tot <- best_model_tot$df
n_tot <- best_model_tot$nobs
AICc_tot <- -tLL_tot+2*k_tot+2*k_tot*(k_tot+1)/(n_tot-k_tot-1)
AICc_tot

coef(best_model_tot)

#use fitted best model to make predictions
y_predicted_tot <- predict(best_model_tot, s = best_lambda_tot, newx = x_test_tot)

plot(y_predicted_tot, test_y_tot, xlab="predicted", ylab="actual")
abline(a=0,b=1, col= 'red')

#find SST and SSE
sst_tot <- sum((test_y_tot - mean(test_y_tot))^2)
sse_tot <- sum((y_predicted_tot - test_y_tot)^2)

#find R-Squared
rsq_tot <- 1 - sse_tot/sst_tot
rsq_tot # R-squared for test data predictions is 0.673

adjR2_tot = 1-(((1-rsq_tot)*(n_tot))/(n_tot-k_tot-1))
adjR2_tot

