#Let use a LASSO for the California data
setwd("/Users/saipyneni/Documents/Conagra Project/Conagra Project/Data")
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
neGardein <- gardein[gardein$neDum == 1,]
#neGardein$logSales <- log(neGardein$unit_sales)
#neGardein$logUnitPrice <- log(neGardein$price_per_unit)

#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample_ne <- sample.split(neGardein$unit_sales, SplitRatio = 0.7)
train_ne  <- subset(neGardein, sample = TRUE)
test_ne   <- subset(neGardein, sample = FALSE)

#define response variable
test_y_ne <- test_ne$unit_sales
train_y_ne <- train_ne$unit_sales

#define matrix of predictor variables
x_test_ne <- model.matrix(unit_sales ~ price_per_unit + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
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
                         pattyDum:price_per_unit + slicedDum :price_per_unit, data = test_ne)


x_train_ne <- model.matrix(unit_sales ~ price_per_unit + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
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
                             pattyDum:price_per_unit + slicedDum :price_per_unit, data = train_ne)



#perform k-fold cross-validation to find optimal lambda value
cv_model_ne <- cv.glmnet(x_train_ne, train_y_ne, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda_ne <- cv_model_ne$lambda.min
best_lambda_ne

plot(cv_model_ne)
summary(cv_model_ne)

#MSE
mse.min_ne <-min(cv_model_ne$cvm)
mse.min_ne
#MSE for the LASSO is 2748370

best_model_ne <- glmnet(x_test_ne, test_y_ne, alpha = 1, lambda = best_lambda_ne)
coef(best_model_ne)

tLL_ne <- best_model_ne$nulldev - deviance(best_model_ne)
k_ne <- best_model_ne$df
n_ne <- best_model_ne$nobs
AICc_ne <- -tLL_ne+2*k_ne+2*k_ne*(k_ne+1)/(n_ne-k_ne-1)
AICc_ne



#use fitted best model to make predictions
y_predicted_ne <- predict(best_model_ne, s = best_lambda_ne, newx = x_test_ne)


plot(y_predicted_ne,test_y_ne,
     xlab="predicted",ylab="actual")
abline(a=0,b=1, col= 'red')

#find SST and SSE
sst_ne <- sum((test_y_ne - mean(test_y_ne))^2)
sse_ne <- sum((y_predicted_ne - test_y_ne)^2)

#find R-Squared
rsq_ne <- 1 - sse_ne/sst_ne
rsq_ne #
#adj r-sq
adjR2_ne = 1-(((1-rsq_ne)*(n_ne-1))/(n_ne-k_ne-1)) 
adjR2_ne #

