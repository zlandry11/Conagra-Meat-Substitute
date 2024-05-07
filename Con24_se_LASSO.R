#Let use a LASSO for the Southest data
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
seGardein <- gardein[gardein$seDum == 1,]

#seGardein$logSales <- log(seGardein$unit_sales)
#seGardein$logUnitPrice <- log(seGardein$price_per_unit)


#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample_se <- sample.split(seGardein$unit_sales, SplitRatio = 0.7)
train_se  <- subset(seGardein, sample_se == TRUE)
test_se   <- subset(seGardein, sample_se == FALSE)

#define response variable
test_y_se <- test_se$unit_sales
train_y_se <- train_se$unit_sales

#define matrix of predictor variables
x_test_se <- model.matrix(unit_sales ~ price_per_unit + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
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
                            pattyDum:price_per_unit + slicedDum :price_per_unit, data = test_se)


x_train_se <- model.matrix(unit_sales ~ price_per_unit + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
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
                            ,  data = train_se)



#perform k-fold cross-validation to find optimal lambda value
cv_model_se <- cv.glmnet(x_train_se, train_y_se, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda_se <- cv_model_se$lambda.min
best_lambda_se

plot(cv_model_se)
summary(cv_model_se)

#MSE
mse.min.se <-min(cv_model_se$cvm)
mse.min.se
#MSE for the LASSO is 4330975


best_model_se <- glmnet(x_test_se, test_y_se, alpha = 1, lambda = best_lambda_se)
coef(best_model_se)
#flvFresh, tye of substitue pork, patty-Texmex,filet-Asian, unitprice in 2021, sliced meats, and asian flavored are insignificant and have been take out of the model 

tLL_se <- best_model_se$nulldev - deviance(best_model_se)
k_se <- best_model_se$df
n_se <- best_model_se$nobs
AICc_se <- -tLL_se+2*k_se+2*k_se*(k_se+1)/(n_se-k_se-1)
AICc_se

#use fitted best model to make predictions
y_predicted_se <- predict(best_model_se, s = best_lambda_se, newx = x_train_se)


plot(y_predicted_se,train_y_se,
     xlab="predicted",ylab="actual", col ='black')
abline(a=0,b=1, col= 'red')

#find SST and SSE
sst_se <- sum((train_y_se - mean(train_y_se))^2)
sse_se <- sum((y_predicted_se - train_y_se)^2)

#find R-Squared, with 52 0.6
rsq_se <- 1 - sse_se/sst_se
rsq_se #


#adjR2
adjR2_se = 1-(((1-rsq_se)*(n_se-1))/(n_se-k_se-1))
adjR2_se #
