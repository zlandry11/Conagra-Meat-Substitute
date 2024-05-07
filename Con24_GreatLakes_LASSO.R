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
glGardein <- gardein[gardein$glDum == 1,]
#glGardein$logSales <- log(glGardein$unit_sales)
#glGardein$logUnitPrice <- log(glGardein$price_per_unit)

#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample_gl <- sample.split(glGardein$unit_sales, SplitRatio = 0.7)
train_gl  <- subset(glGardein, sample = TRUE)
test_gl   <- subset(glGardein, sample = FALSE)

#define response variable
test_y_gl <- test_gl$unit_sales
train_y_gl <- train_gl$unit_sales

#define matrix of predictor variables
x_test_gl <- model.matrix(unit_sales ~ price_per_unit + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
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
                            pattyDum:price_per_unit + slicedDum :price_per_unit, data = test_gl)


x_train_gl <- model.matrix(unit_sales ~ price_per_unit + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
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
                             pattyDum:price_per_unit + slicedDum :price_per_unit, data = train_gl)



#perform k-fold cross-validation to find optimal lambda value
cv_model_gl <- cv.glmnet(x_train_gl, train_y_gl, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda_gl <- cv_model_gl$lambda.min
best_lambda_gl

plot(cv_model_gl)
summary(cv_model_gl)

#MSE
mse.min_gl <-min(cv_model_gl$cvm)
mse.min_gl


best_model_gl <- glmnet(x_test_gl, test_y_gl, alpha = 1, lambda = best_lambda_gl)
coef(best_model_gl)

tLL_gl <- best_model_gl$nulldev - deviance(best_model_gl)
k_gl <- best_model_gl$df
n_gl <- best_model_gl$nobs
AICc_gl <- -tLL_gl+2*k_gl+2*k_gl*(k_gl+1)/(n_gl-k_gl-1)
AICc_gl



#use fitted best model to make predictions
y_predicted_gl <- predict(best_model_gl, s = best_lambda_gl, newx = x_test_gl)


plot(y_predicted_gl,test_y_gl,
     xlab="predicted",ylab="actual")
abline(a=0,b=1, col= 'red')

#find SST and SSE
sst_gl <- sum((test_y_gl - mean(test_y_gl))^2)
sse_gl <- sum((y_predicted_gl - test_y_gl)^2)

#find R-Squared
rsq_gl <- 1 - sse_gl/sst_gl
rsq_gl #

#adj r-sq
adjR2_gl = 1-(((1-rsq_gl)*(n_gl-1))/(n_gl-k_gl-1)) 
adjR2_gl #
