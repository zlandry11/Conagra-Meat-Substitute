#Let use a LASSO for the California data
#setwd("/Users/saipyneni/Documents/Conagra Project/Conagra Project/Data")
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
scGardein <- gardein[gardein$scDum == 1,]
#scGardein_LASSO$logSales <- log(scGardein_LASSO$unit_sales)
#scGardein_LASSO$logUnitPrice <- log(scGardein_LASSO$price_per_unit)

set.seed(1)

sample <- sample.split(scGardein$unit_sales, SplitRatio = 0.7)
train_sc <- subset(scGardein, sample == TRUE)
test_sc <- subset(scGardein, sample == FALSE)

test_y_sc <- test_sc$unit_sales
train_y_sc <- train_sc$unit_sales

x_test_sc <- model.matrix(unit_sales ~ price_per_unit + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
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
                             pattyDum:price_per_unit + slicedDum :price_per_unit, data = test_sc)


x_train_sc <- model.matrix(unit_sales ~ price_per_unit + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
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
                             pattyDum:price_per_unit + slicedDum :price_per_unit
                            ,  data = train_sc)

cv_model_sc <- cv.glmnet(x_train_sc, train_y_sc, alpha = 1)

best_lamda_sc <- cv_model_sc$lambda.min
best_lamda_sc

plot(cv_model_sc)
summary(cv_model_sc)

mse.min.sc <- min(cv_model_sc$cvm)
print(mse.min.sc)
#best mse is 1186774


best_model_sc <- glmnet(x_test_sc, test_y_sc, alpha = 1, lambda = best_lamda_sc)
coef(best_model_sc)

tLL_sc <- best_model_sc$nulldev - deviance(best_model_sc)
k_sc <- best_model_sc$df
n_sc <- best_model_sc$nobs
AICc_sc <- -tLL_sc + 2*k_sc + 2*k_sc*(k_sc+1)/(n_sc-k_sc-1)
AICc_sc

y_predicted <- predict(best_model_sc, s = best_lamda_sc, newx = x_train_sc)
#ncol(x_train)

plot(y_predicted, train_y_sc, xlab = "predicted", ylab = "actual")
abline(a=0,b=1, col = 'red')

sst_sc <- sum((train_y_sc - mean(train_y_sc))^2)
sse_sc <- sum((y_predicted - train_y_sc)^2)

rsq_sc <- 1 - sse_sc/sst_sc
print(rsq_sc)
#adjR2
adjR2_sc = 1-(((1-rsq_sc)*(n_sc-1))/(n_sc-k_sc-1))
adjR2_sc #0.7563741

