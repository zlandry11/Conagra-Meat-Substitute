#Let use a LASSO for the Plains data
setwd("/Users/saipyneni/Documents/Conagra Project/Conagra Project/Data")
rm(list=ls()) #Clear up the Working Environment
# Libraries ---------------------------------------------------------------

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

# Import Data ---------------------------------------------------------------
gardein <- read.csv("gardeinCleaned.csv")
plnGardein <- gardein[gardein$plainDum == 1,]

#plnGardein$logSales <- log(plnGardein$unit_sales)
#plnGardein$logUnitPrice <- log(plnGardein$price_per_unit)


#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample_plain <- sample.split(plnGardein$logSales, SplitRatio = 0.7)
train_plain  <- subset(plnGardein, sample_plain == TRUE)
test_plain   <- subset(plnGardein, sample_plain == FALSE)

#define response variable
test_y_plain <- test_plain$unit_sales
train_y_plain <- train_plain$unit_sales

#define matrix of predictor variables
x_test_plain <- model.matrix(unit_sales ~ price_per_unit+ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
                             + ppuUnitRedOnly + poulDum + nonMeatDum + seaDum + filetDum + grndDum + meatballDum + nuggetDum + 
                               pattyDum + slicedDum + flvAsianDum + flvBfstDum + flvFishnDum + flvFreshnDum + flvGrainDum + flvItlnDum
                             + flvRglrDum + flvSpicyDum + flvTxMxDum + tosBeefDum + tosChkDum  + tosPrkDum + tosSfDum + tosSsgDUm
                             + tosUnkDum + y20 + y21 + y22 + y23 + price_per_unit:y20 + price_per_unit:y21 + price_per_unit:y22
                             + price_per_unit:y23 + price_per_unit:ppuDispOnly + price_per_unit:ppuFeatNDisp 
                             + price_per_unit:ppuFeatOnly + price_per_unit:ppuNoMerch + price_per_unit:ppuUnitRedOnly
                             + filetDum :flvRglrDum + filetDum :flvAsianDum 
                             + nuggetDum:flvRglrDum + nuggetDum:flvTxMxDum + nuggetDum:flvGrainDum + nuggetDum: flvSpicyDum + nuggetDum:flvAsianDum
                             + pattyDum:flvRglrDum + pattyDum:flvTxMxDum + pattyDum:flvItlnDum + pattyDum:flvSpicyDum + pattyDum:flvFreshnDum + 
                               pattyDum:flvBfstDum + slicedDum:flvItlnDum + slicedDum:flvRglrDum 
                             + poulDum:price_per_unit + nonMeatDum:price_per_unit + seaDum:price_per_unit + filetDum:price_per_unit
                             + grndDum:price_per_unit + meatballDum:price_per_unit + nuggetDum:price_per_unit + 
                               pattyDum:price_per_unit + slicedDum :price_per_unit
                       , data = test_plain)


x_train_plain <- model.matrix(unit_sales ~ price_per_unit + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
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
                        ,  data = train_plain)



#perform k-fold cross-validation to find optimal lambda value. Default is set to 10 folds
cv_model_plain <- cv.glmnet(x_train_plain, train_y_plain, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda_plain <- cv_model_plain$lambda.min
best_lambda_plain

plot(cv_model_plain)
summary(cv_model_plain)

#MSE The mean cross-validated error - a vector of length length(lambda).
mse.min.plain <-min(cv_model_plain$cvm)
mse.min.plain
#MSE for the LASSO is 231798.8


best_model_plain <- glmnet(x_test_plain, test_y_plain, alpha = 1, lambda = best_lambda_plain)
coef(best_model_plain)

#flvFishN, type of susbstitue sausage, Asian flavored nuggest, texmex patties, italian sliced meat are insignificant and take out of the model

tLL_plain <- best_model_plain$nulldev - deviance(best_model_plain)
k_plain <- best_model_plain$df
n_plain <- best_model_plain$nobs
AICc_plain <- -tLL_plain+2*k_plain+2*k_plain*(k_plain+1)/(n_plain-k_plain-1)
AICc_plain

#use fitted best model to make predictions
y_predicted_plain <- predict(best_model_plain, s = best_lambda_plain, newx = x_train_plain)


plot(y_predicted_plain,train_y_plain,
     xlab="predicted",ylab="actual")
abline(a=0,b=1, col= 'red')

#find SST and SSE
sst_plain <- sum((train_y_plain - mean(train_y_plain))^2)
sse_plain <- sum((y_predicted_plain - train_y_plain)^2)

#find R-Squared, with 52 0.64
rsq_plain <- 1 - sse_plain/sst_plain
rsq_plain #


#adjR2
adjR2_plain = 1-(((1-rsq_plain)*(n_plain-1))/(n_plain-k_plain-1))
adjR2_plain #

