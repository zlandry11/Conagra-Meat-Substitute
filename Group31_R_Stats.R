#Change the directory to where the data is located
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
gardein <- read.csv("gardeinCleaned.csv")


dfN24 <- gardein[gardein$Year < 2024,]

calGardein <- gardein[gardein$calDum == 1,]



options("scipen" = 15)


#grah histogram of our Y, unit_sales showing its skewed right
hist(gardein$unit_sales, breaks = 100, main = "Histogram of Unit Sales", xlab= "Unit Sales")

#we should log transform it to normalize
gardein$logSales <- log(gardein$unit_sales)

#now histogram shows a more normal distribution to be used in our regression
hist(gardein$logSales, breaks = 75, main = "Histogram of Unit Sales (Log Transformed)", xlab= "Unit Sales")

#let's check for price per unit as well
hist(gardein$price_per_unit,breaks = 75, main = "Histogram of Price Per Unit", xlab= "Per/Unit")

hist(log(gardein$price_per_unit),breaks = 75, main = "Histogram of Price Per Unit", xlab= "Per/Unit")

#also skewed right, let's log transform this variable as well
gardein$logUnitPrice <- log(gardein$price_per_unit)

#check the histogram again
hist(gardein$logUnitPrice, breaks = 75, main = "Histogram of Price Per Unit (Log Transformed)", xlab= "Price/Unit")


attach(gardein)
plot((price_per_unit), unit_sales, main="Price vs Unit Sales Total US (Log)",
     xlab="Price Per Unit ", ylab="Unit Sales ", pch=20)

plot(logUnitPrice, logSales, main="Scatterplot Price vs Unit Sales",
     xlab="Price Per Unit ", ylab="Unit Sales ", pch=20)

pairs(~logSales+logUnitPrice+product_type_id+form_id+type_of_meat_substituted_id+flavor_id,data=gardein,
      main="Simple Scatterplot Matrix")

#regression without using transformed variables unit_sale as a function of price per unit
regGeoPrice <- lm(unit_sales~ price_per_unit, data = gardein)
summary(regGeoPrice)
#getting r^2 as 0.02095

plot(logSales~logUnitPrice, data=gardein)


#Regression using transformed variables for price and unit sales
regTGeoPrice <- lm(logSales ~  logUnitPrice + (price_per_unit^2), data = gardein)
summary(regTGeoPrice)
#R^2 decreased to 0.04479 Other variables need to be broought in to look at variance in data, 
#we can consider looking at total us sales first and only looking at pre 2024 data. 



#let us look at our regression again 
regTGeoPrice2 <- lm(logSales ~ logUnitPrice, data = dfN24)
summary(regTGeoPrice2)




#regression with price and region interaction with
regPriceRegion <- lm(logSales ~ logUnitPrice + calDum + plainDum + seDum + neDum + glDum + scDum + msDum + westDum 
                     + I(logUnitPrice * calDum) + I(logUnitPrice * plainDum) + I(logUnitPrice * seDum) 
                     + I(logUnitPrice*neDum) + I(logUnitPrice*scDum) +I(logUnitPrice*msDum)
                     + I(logUnitPrice*westDum) + I(logUnitPrice* glDum), data = gardein
)
summary(regPriceRegion)



#regression with above plus the promotions without ppuSpecPackOnly as it does not have any data
regPriceRegionPromo <- lm(logSales ~ logUnitPrice + ppuAnyMerch + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
                          + ppuUnitRedOnly +
                            + calDum + plainDum + seDum + neDum + glDum + scDum + msDum + westDum 
                          + I(logUnitPrice * calDum) + I(logUnitPrice * plainDum) + I(logUnitPrice * seDum) 
                          + I(logUnitPrice*neDum) + I(logUnitPrice*scDum) +I(logUnitPrice*msDum)
                          + I(logUnitPrice*westDum) + I(logUnitPrice* glDum) +  poulDum + nonMeatDum + seaDum 
                          + filetDum + grndDum + meatballDum + nuggetDum + + flvAsianDum + flvBfstDum + flvFishnDum 
                          + flvFreshnDum + flvGrainDum + flvItlnDum
                          + flvRglrDum + flvSpicyDum + flvTxMxDum + tosBeefDum + tosChkDum  + tosPrkDum + tosSfDum + tosSsgDUm
                          + tosUnkDum + + y20 + y21 + y22 + y23 + I(logUnitPrice * ppuAnyMerch ) +
                            pattyDum + slicedDum, data = gardein)
summary(regPriceRegionPromo)

plot(predict(regPriceRegionPromo),gardein$logSales,
     xlab="predicted",ylab="actual")
abline(a=0,b=1, col="red",lwd=2)


#Let's take a look at California, but add flavor 

calGardein <- gardein[gardein$calDum == 1,]
calPalPromo <- lm(logSales ~  logUnitPrice + ppuAnyMerch + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
                  + ppuUnitRedOnly + poulDum + nonMeatDum + seaDum + filetDum + grndDum + meatballDum + nuggetDum + 
                    pattyDum + slicedDum + flvAsianDum + flvBfstDum + flvFishnDum + flvFreshnDum + flvGrainDum + flvItlnDum
                  + flvRglrDum + flvSpicyDum + flvTxMxDum + tosBeefDum + tosChkDum  + tosPrkDum + tosSfDum + tosSsgDUm
                  + tosUnkDum + y20 + y21 + y22 + y23 + I(logUnitPrice * y20) + I(logUnitPrice*y21) + I(logUnitPrice*y22)
                  + I(logUnitPrice * y23) + I(logUnitPrice*ppuAnyMerch)  + I(logUnitPrice * ppuDispOnly) + I(logUnitPrice*ppuFeatNDisp) 
                  + I(logUnitPrice*ppuFeatOnly) + I(logUnitPrice*ppuNoMerch) + I(logUnitPrice * ppuUnitRedOnly)
                  , data = calGardein)
calModSum <- summary(calPalPromo)
calModSum

vif(lm(logSales ~   ppuAnyMerch + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
       + ppuUnitRedOnly + poulDum + nonMeatDum + seaDum + filetDum + grndDum + meatballDum + nuggetDum + 
         pattyDum + slicedDum + flvAsianDum + flvBfstDum + flvFishnDum + flvFreshnDum + flvGrainDum + flvItlnDum
       + flvRglrDum + flvSpicyDum + flvTxMxDum + tosBeefDum + tosChkDum  + tosPrkDum + tosSfDum + tosSsgDUm
       + tosUnkDum
       , data = calGardein))
vif_values <- vif(calPalPromo)
vif_values
#unit price*y23, unit price*PPU Unit Red Only & unitFeature only are not significant 

mseCal <- mean(calModSum$residuals^2)
#MSE I am getting the regression model is 1.473
#adjusted r^2 was 0.6494

AIC(calPalPromo)

plot(predict(calPalPromo),calGardein$logSales,
     xlab="predicted",ylab="actual")
abline(a=0,b=1, col="red",lwd=2)








#Let's take look at Southeast  
seGardein <- gardein[gardein$seDum == 1,]
sePromo <- lm(logSales ~  logUnitPrice+ ppuAnyMerch + ppuDispOnly + ppuFeatNDisp + ppuFeatOnly + ppuNoMerch 
                  + ppuUnitRedOnly + poulDum + nonMeatDum + seaDum + filetDum + grndDum + meatballDum + nuggetDum + 
                    pattyDum + slicedDum + flvAsianDum + flvBfstDum + flvFishnDum + flvFreshnDum + flvGrainDum + flvItlnDum
                  + flvRglrDum + flvSpicyDum + flvTxMxDum + tosBeefDum + tosChkDum  + tosPrkDum + tosSfDum + tosSsgDUm
                  + tosUnkDum + y20 + y21 + y22 + y23 + I(logUnitPrice * y20) + I(logUnitPrice*y21) + I(logUnitPrice*y22)
                  + I(logUnitPrice * y23) + I(logUnitPrice*ppuAnyMerch)  + I(logUnitPrice * ppuDispOnly) + I(logUnitPrice*ppuFeatNDisp) 
                  + I(logUnitPrice*ppuFeatOnly) + I(logUnitPrice*ppuNoMerch) + I(logUnitPrice * ppuUnitRedOnly)
                  , data = seGardein)
seModSum <- summary(sePromo)
seModSum


#FORM Exclude MEAT, NO LINK, NO OTHER FORM EITHER, 
#FOR FLAVOR, NO VEGETABLE, NO OTHER, NO LUNCH MEAT, NO INDIAN, NO HERB, NO CHEESE, NO BARBEQUE offered
#FOR TYPE OF MEAT SUBSTITUED: NO SANDWHICH, NON MEAT, NO MEATBALL, NO BURGER
#Year is not significant
#---------- ANOVA Testing-------------------------------------------------------

# Filter out the "TotalUS" category
gardein_noTotalUS <- gardein %>% filter(geography != "TotalUS")

# Unit Sales vs. Region
# Box plot of unit sales for each region to visually analyze variances
ggplot(data = gardein_noTotalUS, aes(x = geography, y = logSales, fill = geography))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", size=2, color="black", fill="black") +
  xlab("Region")+
  ylab("Sales")+
  ggtitle("Unit Sales by Region")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
# Use Levene's Test to check homogeneity of variances
library(car)
LeveneTest(logSales~geography, data = gardein_noTotalUS) # p-value indicates variances are significantly different
# Welch ANOVA
oneway.test(logSales~geography, data = gardein_noTotalUS, var.equal = FALSE)

# Unit Sales vs. Season
# Box plot of unit sales for each season to visually analyze variances
ggplot(data = gardein, aes(x = Season, y = logSales, fill = Season))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", size=2, color="black", fill="black") +
  xlab("Season")+
  ylab("Sales")+
  ggtitle("Unit Sales by Season")+
  theme(legend.position = "none")
# Use Levene's Test to check homogeneity of variances
LeveneTest(logSales~Season, data = gardein) # p-value indicates variances are not significantly different
# ANOVA
season.aov <- aov(logSales~Season, data = gardein)
summary(season.aov)

# Unit Sales vs. Form
# Box plot of unit sales for each season to visually analyze variances
ggplot(data = gardein, aes(x = form_cat, y = logSales, fill = form_cat))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", size=2, color="black", fill="black") +
  xlab("Form")+
  ylab("Sales")+
  ggtitle("Unit Sales by Form")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
# Use Levene's Test to check homogeneity of variances
LeveneTest(logSales~form_cat, data = gardein) # p-value indicates variances are significantly different
# Welch ANOVA
oneway.test(logSales~form_cat, data = gardein, var.equal = FALSE)

# Unit Sales vs. Flavor
# Box plot of unit sales for each season to visually analyze variances
ggplot(data = gardein, aes(x = flavor_cat, y = logSales, fill = flavor_cat))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", size=2, color="black", fill="black") +
  xlab("Flavor")+
  ylab("Sales")+
  ggtitle("Unit Sales by Flavor")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
# Use Levene's Test to check homogeneity of variances
LeveneTest(logSales~flavor_cat, data = gardein) # p-value indicates variances are significantly different
# Welch ANOVA
oneway.test(logSales~flavor_cat, data = gardein, var.equal = FALSE)


