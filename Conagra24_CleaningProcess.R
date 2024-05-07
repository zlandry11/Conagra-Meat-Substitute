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
#Attribute Sheet
attProd <- read_excel("Product Attributes.xlsx")



# SUBSTITUTE MEATS FROZEN ----

fzSubMeatPos20 <-read_excel("Fz_Rfg Substitute Meat_POS_2020.xlsx")
fzSubMeatPos21 <-read_excel("Fz_Rfg Substitute Meat_POS_2021.xlsx")
fzSubMeatPos22 <-read_excel("Fz_Rfg Substitute Meat_POS_2022.xlsx")
fzSubMeatPos23 <-read_excel("Fz_Rfg Substitute Meat_POS_2023.xlsx")
fzSubMeatPos24 <-read_excel("Fz_Rfg Substitute Meat_POS_2024.xlsx")

# Code ----

# Subsitute Meats into subsDf dataframe ----
sb = list(fzSubMeatPos20,
          fzSubMeatPos21,
          fzSubMeatPos22,
          fzSubMeatPos23,
          fzSubMeatPos24)

subsDf = bind_rows(sb, .id = "column_label")

subsDf <- merge(subsDf, attProd, by = "UPC 13 digit")

subsDf <- subsDf[,which(
  names(subsDf) %in% c(
  #  "Product.x",
    "UPC 13 Digit",
    "Geography",
    "Time",
    "Unit Sales",
    "Unit Sales No Merch",
    "Unit Sales Any Merch",
    "Unit Sales Price Reductions Only",
    "Unit Sales Feature Only",
    "Unit Sales Display Only",
    "Unit Sales Special Pack Only",
    "Unit Sales Feature and Display",
    "Base Unit Sales",
    "Incremental Units",
    "Price per Unit",
    "Price per Unit No Merch",
    "Price per Unit Any Merch",
    "Price per Unit Price Reductions Only",
    "Price per Unit Feature Only",
    "Price per Unit Display Only",
    "Price per Unit Special Pack Only",
    "Price per Unit Feature and Display",
    #"UPC 13 digit",
    #"Category Name",
   # "Sub-Category Name",
    "Brand Name",
   # "Brand Franchise Name",
    "Package",
    "Form",
    "Flavor / Scent",
    "Meat Source",
    "Product Type",
    "Type Of Meat Substituted",
    "Type Of Substitute",
    "Cooked Info"
  )
)]



#Remove "Week Ending" in Time Column
subsDf = subsDf%>% mutate(across('Time', str_replace, 'Week Ending ', ''))



#run four times to remove '-'
subsDf<-subsDf%>% mutate(across('Time', str_replace, '-', ''))

#Covert Time column to date
subsDf$Time <- as.Date(subsDf$Time , format = "%m%d%y")


#Condense geography field 
subsDf<-subsDf%>% mutate(across('Geography', str_replace, 'Standard ', ''))
subsDf<-subsDf%>% mutate(across('Geography', str_replace, 'Multi Outlet ', ''))
subsDf<-subsDf%>% mutate(across('Geography', str_replace, 'Conv', ''))
subsDf<-subsDf%>% mutate(across('Geography', str_replace, '\\+ ', ''))
subsDf<-subsDf%>% mutate(across('Geography', str_replace, '-', ''))
subsDf<-subsDf%>% mutate(across('Geography', str_replace, '_', ''))
subsDf<-subsDf%>% mutate(across('Geography', str_replace, ' ', ''))




#Fix columnNames
install.packages("janitor")
library(janitor)
#can be done by simply
subsDf <- clean_names(subsDf)

#Getting only Gardein
gardein <- subsDf

library(data.table)
setDT(gardein)

# Categorization ----------------------------------------------------------
#Gardein or Not
gardein$gardein = ""
gardein$gardein <- ifelse(gardein$brand_name %in% c("GARDEIN"), 1, 0)

#Gardein Only
gardein <- gardein[gardein == 1,]


library(data.table)
setDT(gardein)
write.csv(gardein, "/Users/saipyneni/Documents/Conagra Project/Conagra Project/Data/gardeinCleaned.csv", row.names=FALSE)
gardein <- read.csv("gardeinCleaned.csv")


#Cateogrizartion & Standardization Product Type
gardein$product_type_id = ""
gardein$product_type_cat = ""


gardein[product_type %in% c("TOFU",
"SOY",
"TEMPEH",
"SEITAN",
"FALAFEL",
"PLANT BASED SUBSTITUTE",
"TOFU SUBSTITUTE",
"SOY SUBSTITUTE",
"TEMPEH SUBSTITUTE"), product_type_id := 0]


gardein[product_type %in% c("TOFU",
                            "SOY",
                            "TEMPEH",
                            "SEITAN",
                            "FALAFEL",
                            "PLANT BASED SUBSTITUTE",
                            "TOFU SUBSTITUTE",
                            "SOY SUBSTITUTE",
                            "TEMPEH SUBSTITUTE"), product_type_cat := "NON-MEAT"]


gardein[product_type %in% c("MEAT SUBSTITUTE", "BURGER",
"VEGETABLE MEAT SUBSTITUTE"), product_type_id := 1]

gardein[product_type %in% c("MEAT SUBSTITUTE", "BURGER",
                            "VEGETABLE MEAT SUBSTITUTE"), product_type_cat := "MEAT"]

gardein[product_type %in% c("FISH SUBSTITUTE",
"SEAFOOD SUBSTITUTE"), product_type_id := 2]

gardein[product_type %in% c("FISH SUBSTITUTE",
                            "SEAFOOD SUBSTITUTE"), product_type_cat := "SEAFOOD"]

gardein[product_type %in% c("POULTRY SUBSTITUTE",
"CHICKEN SUBSTITUTE",
"PLANT BASED CHICKEN SUBSTITUTE",
"PLANT BASED TURKEY SUBSTITUTE"), product_type_id := 3]

gardein[product_type %in% c("POULTRY SUBSTITUTE",
                             "CHICKEN SUBSTITUTE",
                             "PLANT BASED CHICKEN SUBSTITUTE",
                             "PLANT BASED TURKEY SUBSTITUTE"), product_type_cat := "POULTRY"]



#Cateogrizartion & Standardization Form 
gardein$form_id = ""
gardein$form_cat = ""

gardein[form %in% c("STRIP",
"FILET",
"CUTLET",
"BREAST",
"ROAST",
"STEAK",
"SPARE RIBS",
"RIBLET",
"CUT",
"MEAT LOAF"), form_id := 0]


gardein[form %in% c("STRIP",
                    "FILET",
                    "CUTLET",
                    "BREAST",
                    "ROAST",
                    "STEAK",
                    "SPARE RIBS",
                    "RIBLET",
                    "CUT",
                    "MEAT LOAF"), form_cat := "FILET/STRIP"]


gardein[form %in% c("CRUMBLE",
"GROUND",
"DICED",
"SHREDS",
"SHREDDED"), form_id := 1]


gardein[form %in% c("CRUMBLE",
                    "GROUND",
                    "DICED",
                    "SHREDS",
                    "SHREDDED"), form_cat := "GROUND"]


gardein[form %in% c("LINK",
"FRANK",
"BREAKFAST LINK",
"SAUSAGE",
"HOT DOG",
"BRATWURST",
"DINNER LINK",
"BREAKFAST SAUSAGE LINK",
"DINNER SAUSAGE LINK",
"BREAKFAST SAUSAGE ROLL"), form_id := 2]

gardein[form %in% c("LINK",
                    "FRANK",
                    "BREAKFAST LINK",
                    "SAUSAGE",
                    "HOT DOG",
                    "BRATWURST",
                    "DINNER LINK",
                    "BREAKFAST SAUSAGE LINK",
                    "DINNER SAUSAGE LINK",
                    "BREAKFAST SAUSAGE ROLL"), form_cat := "LINK"]

gardein[form %in% c("MEATBALL",
"BALL"), form_id := 3]

gardein[form %in% c("MEATBALL",
                    "BALL"), form_cat := "MEATBALL"]

gardein[form %in% c("NUGGET",
"TENDERS",
"WING",
"TENDER",
"BITE",
"DIPPER",
"POPPER",
"FINGER",
"FUN NUGGETS"), form_id := 4]
gardein[form %in% c("NUGGET",
                    "TENDERS",
                    "WING",
                    "TENDER",
                    "BITE",
                    "DIPPER",
                    "POPPER",
                    "FINGER",
                    "FUN NUGGETS"), form_cat := "NUGGET / TENDER"]

gardein[form %in% c("NPATTY",
                    "PATTY",
"BURGER",
"BURGER PATTY",
"SLIDER",
"BREAKFAST PATTY",
"BREAKFAST SAUSAGE PATTY",
"SAUSAGE PATTY"), form_id := 6]

gardein[form %in% c("NPATTY",
                    "PATTY",
                    "BURGER",
                    "BURGER PATTY",
                    "SLIDER",
                    "BREAKFAST PATTY",
                    "BREAKFAST SAUSAGE PATTY",
                    "SAUSAGE PATTY"), form_cat := "PATTY / BURGER"]

gardein[form %in% c("SLICED",
"SLICE",
"DELI SLICED",
"ULTRA THIN SLICE"), form_id := 7]

gardein[form %in% c("SLICED",
                    "SLICE",
                    "DELI SLICED",
                    "ULTRA THIN SLICE"), form_cat := "SLICED"]


gardein[form_id %in% c(""), form_id := 5]
gardein[form_cat %in% c(""), form_cat := "OTHER"]



gardein$type_of_meat_substituted_id = ""
gardein$type_of_meat_substituted_cat = ""



gardein[type_of_meat_substituted %in% c("BEEF",
"CARNE ASADA STEAK",
"MEATLOAF",
"STEAK",
"CORNED BEEF",
"ROAST BEEF"), type_of_meat_substituted_id := 0]

gardein[type_of_meat_substituted %in% c("BEEF",
                                        "CARNE ASADA STEAK",
                                        "MEATLOAF",
                                        "STEAK",
                                        "CORNED BEEF",
                                        "ROAST BEEF"), type_of_meat_substituted_cat := "BEEF"]


gardein[type_of_meat_substituted %in% c("BURGER"
                                        ), type_of_meat_substituted_id := 1]

gardein[type_of_meat_substituted %in% c("BURGER"
                                        ), type_of_meat_substituted_cat := "BURGER"]

gardein[type_of_meat_substituted %in% c("CHICKEN",
"CHICKEN AND PORK"), type_of_meat_substituted_id := 2]

gardein[type_of_meat_substituted %in% c("CHICKEN",
                                        "CHICKEN AND PORK"), type_of_meat_substituted_cat := "CHICKEN"]

gardein[type_of_meat_substituted %in% c("MEATBALL",
                                        "ITALIAN MEATBALL"), type_of_meat_substituted_id := 3]

gardein[type_of_meat_substituted %in% c("MEATBALL",
                                        "ITALIAN MEATBALL"), type_of_meat_substituted_cat := "MEATBALL"]


gardein[type_of_meat_substituted %in% c("PORK",
"BACON",
"HAM",
"PORK BACON"), type_of_meat_substituted_id := 4]
gardein[type_of_meat_substituted %in% c("PORK",
                                        "BACON",
                                        "HAM",
                                        "PORK BACON"), type_of_meat_substituted_cat := "PORK"]

gardein[type_of_meat_substituted %in% c("PASTRAMI",
"SALAMI",
"ITALIAN DELI MEAT"), type_of_meat_substituted_id := 5]

gardein[type_of_meat_substituted %in% c("PASTRAMI",
                                        "SALAMI",
                                        "ITALIAN DELI MEAT"), type_of_meat_substituted_cat := "SANDWICH"]


gardein[type_of_meat_substituted %in% c("SAUSAGE",
"ITALIAN SAUSAGE",
"HOT DOG",
"BRAT",
"ANDOUILLE SAUSAGE",
"CHORIZO",
"PORK SAUSAGE",
"PROSCIUTTO",
"PEPPERONI",
"CARPACCIO",
"LONGANIZA",
"FRANKFURTER",
"BOLOGNA",
"BRATWURST"), type_of_meat_substituted_id := 6]

gardein[type_of_meat_substituted %in% c("SAUSAGE",
                                        "ITALIAN SAUSAGE",
                                        "HOT DOG",
                                        "BRAT",
                                        "ANDOUILLE SAUSAGE",
                                        "CHORIZO",
                                        "PORK SAUSAGE",
                                        "PROSCIUTTO",
                                        "PEPPERONI",
                                        "CARPACCIO",
                                        "LONGANIZA",
                                        "FRANKFURTER",
                                        "BOLOGNA",
                                        "BRATWURST"), type_of_meat_substituted_cat := "SAUSAGE"]


gardein[type_of_meat_substituted %in% c("BELT FISH",
"FISH",
"CRAB",
"SALMON",
"TUNA",
"SHRIMP"), type_of_meat_substituted_id := 7]

gardein[type_of_meat_substituted %in% c("BELT FISH",
                                        "FISH",
                                        "CRAB",
                                        "SALMON",
                                        "TUNA",
                                        "SHRIMP"), type_of_meat_substituted_cat := "SEAFOOD"]


gardein[type_of_meat_substituted %in% c("VALUE NOT AVAILABLE",
                                        "N/A",
                                        "TEMPEH"), type_of_meat_substituted_id := 8]

gardein[type_of_meat_substituted %in% c("VALUE NOT AVAILABLE",
                                        "N/A",
                                        "TEMPEH"), type_of_meat_substituted_cat := "UNKNOWN"]



gardein$flavor_id = ""
gardein$flavor_cat = ""

gardein[flavor_scent %in% c("5 SPICE",
                            "ASIAN",
                            "ASIAN VEGETABLE",
                            "CHICKEN SESAME GARLIC",
                            "CHICKEN THAI BASIL",
                            "GENERAL TSOS",
                            "GINGER",
                            "GINGER SCALLION",
                            "KIMCHI",
                            "KOREAN",
                            "KOREAN BARBEQUE",
                            "KOREAN STYLE BARBEQUE",
                            "MANDARIN ORANGE",
                            "MANGO & BASIL",
                            "SESAME GARLIC",
                            "SESAME GINGER",
                            "SIZZLING SZECHUAN",
                            "SPICY SICHUAN",
                            "SPICY THAI",
                            "SRIRACHA THAI CHILI",
                            "SWEET & SOUR",
                            "SWEET & SPICY PEPPER",
                            "TERIYAKI",
                            "THAI",
                            "THAI COCONUT",
                            "CITRUS SPARERIB"), flavor_id := 0]

gardein[flavor_scent %in% c("5 SPICE",
                            "ASIAN",
                            "ASIAN VEGETABLE",
                            "CHICKEN SESAME GARLIC",
                            "CHICKEN THAI BASIL",
                            "GENERAL TSOS",
                            "GINGER",
                            "GINGER SCALLION",
                            "KIMCHI",
                            "KOREAN",
                            "KOREAN BARBEQUE",
                            "KOREAN STYLE BARBEQUE",
                            "MANDARIN ORANGE",
                            "MANGO & BASIL",
                            "SESAME GARLIC",
                            "SESAME GINGER",
                            "SIZZLING SZECHUAN",
                            "SPICY SICHUAN",
                            "SPICY THAI",
                            "SRIRACHA THAI CHILI",
                            "SWEET & SOUR",
                            "SWEET & SPICY PEPPER",
                            "TERIYAKI",
                            "THAI",
                            "THAI COCONUT",
                            "CITRUS SPARERIB"), flavor_cat := "ASIAN"]

gardein[flavor_scent %in% c("BARBEQUE",
                            "BARBEQUE CHICKEN",
                            "BARBEQUE SAUCED",
                            "BARBEQUE SEASONED",
                            "CHICKEN BARBEQUE",
                            "COAL ROASTED",
                            "GRANDPA MELS BARBEQUE",
                            "K C BARBEQUE",
                            "RASPBERRY HABANERO BARBEQUE",
                            "SMOKED",
                            "SMOKED HICKORY BARBEQUE",
                            "SMOKY",
                            "SMOKY & SPICY",
                            "SMOKY SAUSAGE",
                            "SWEET BARBEQUE",
                            "SWEET BARBEQUE CHICKEN",
                            "WOOD SMOKED"), flavor_id := 1]

gardein[flavor_scent %in% c("BARBEQUE",
                            "BARBEQUE CHICKEN",
                            "BARBEQUE SAUCED",
                            "BARBEQUE SEASONED",
                            "CHICKEN BARBEQUE",
                            "COAL ROASTED",
                            "GRANDPA MELS BARBEQUE",
                            "K C BARBEQUE",
                            "RASPBERRY HABANERO BARBEQUE",
                            "SMOKED",
                            "SMOKED HICKORY BARBEQUE",
                            "SMOKY",
                            "SMOKY & SPICY",
                            "SMOKY SAUSAGE",
                            "SWEET BARBEQUE",
                            "SWEET BARBEQUE CHICKEN",
                            "WOOD SMOKED"), flavor_cat := "BARBEQUE"]

gardein[flavor_scent %in% c("APPLE MAPLE",
                            "APPLE SAUSAGE",
                            "APPLEWOOD SMOKE",
                            "APPLEWOOD SMOKED",
                            "BACON",
                            "BREAKFAST SAUSAGE",
                            "BREAKFAST SCRAMBLE",
                            "CANADIAN BACON",
                            "CHICKEN APPLE SAUSAGE",
                            "HUEVO RANCHEROS BREAKFAST",
                            "MAPLE",
                            "MAPLE SAUSAGE",
                            "SAUSAGE",
                            "SMOKED APPLE SAGE",
                            "SMOKEY SAVORY MAPLE",
                            "SMOKY MAPLE BACON",
                            "SWEET APPLE"), flavor_id := 2]

gardein[flavor_scent %in% c("APPLE MAPLE",
                            "APPLE SAUSAGE",
                            "APPLEWOOD SMOKE",
                            "APPLEWOOD SMOKED",
                            "BACON",
                            "BREAKFAST SAUSAGE",
                            "BREAKFAST SCRAMBLE",
                            "CANADIAN BACON",
                            "CHICKEN APPLE SAUSAGE",
                            "HUEVO RANCHEROS BREAKFAST",
                            "MAPLE",
                            "MAPLE SAUSAGE",
                            "SAUSAGE",
                            "SMOKED APPLE SAGE",
                            "SMOKEY SAVORY MAPLE",
                            "SMOKY MAPLE BACON",
                            "SWEET APPLE"), flavor_cat := "BREAKFAST"]


gardein[flavor_scent %in% c("CHEDDAR",
                            "CHEESE"), flavor_id := 3]

gardein[flavor_scent %in% c("CHEDDAR",
                            "CHEESE"), flavor_cat := "CHEESE"]


gardein[flavor_scent %in% c("FISH",
                            "LEMON DILL SALMON"), flavor_id := 4]

gardein[flavor_scent %in% c("FISH",
                            "LEMON DILL SALMON"), flavor_cat := "FISH"]



gardein[flavor_scent %in% c("BAJA",
                            "CALIFORNIA",
                            "CALIFORNIA STYLE",
                            "CARIBBEAN STYLE PLANTAIN",
                            "CRANBERRY & GOAT CHEESE",
                            "GARDEN",
                            "GARDEN BROILER",
                            "GARDEN FRESH",
                            "JUICY",
                            "LEMON"), flavor_id := 5]

gardein[flavor_scent %in% c("BAJA",
                            "CALIFORNIA",
                            "CALIFORNIA STYLE",
                            "CARIBBEAN STYLE PLANTAIN",
                            "CRANBERRY & GOAT CHEESE",
                            "GARDEN",
                            "GARDEN BROILER",
                            "GARDEN FRESH",
                            "JUICY",
                            "LEMON"), flavor_cat := "FRESH"]

gardein[flavor_scent %in% c("3 GRAIN",
                            "5 GRAIN",
                            "7 GRAIN",
                            "GRAIN & SEED MEDLEY",
                            "LENTIL",
                            "LENTIL BARLEY",
                            "LENTIL SAGE",
                            "LUPINI BEAN",
                            "MULTI GRAIN",
                            "QUINOA",
                            "QUINOA CRUNCH",
                            "ROASTED GARLIC & QUINOA",
                            "SPROUTED QUNIOA CHIA",
                            "SUNRICE",
                            "SUNRISE TRAIL MIX",
                            "SWEET POTATO & RED QUINOA",
                            "WALNUT",
                            "WALNUT & CHEESE"), flavor_id := 6]

gardein[flavor_scent %in% c("3 GRAIN",
                            "5 GRAIN",
                            "7 GRAIN",
                            "GRAIN & SEED MEDLEY",
                            "LENTIL",
                            "LENTIL BARLEY",
                            "LENTIL SAGE",
                            "LUPINI BEAN",
                            "MULTI GRAIN",
                            "QUINOA",
                            "QUINOA CRUNCH",
                            "ROASTED GARLIC & QUINOA",
                            "SPROUTED QUNIOA CHIA",
                            "SUNRICE",
                            "SUNRISE TRAIL MIX",
                            "SWEET POTATO & RED QUINOA",
                            "WALNUT",
                            "WALNUT & CHEESE"), flavor_cat := "GRAIN"]


gardein[flavor_scent %in% c("BASIL PESTO",
                            "FRENCH HERB",
                            "GARDEN HERB",
                            "GARLIC & HERB",
                            "GRAIN & HERB",
                            "HERB & SPICE",
                            "HERB ROASTED",
                            "HERBY GARLIC GREENS",
                            "HICKORY & SAGE",
                            "HICKORY & SAGE SMOKED",
                            "HICKORY SMOKED",
                            "LEMON HERB",
                            "LEMON PEPPER",
                            "PARMESAN GARLIC",
                            "SEASON & LIME",
                            "SEASONED",
                            "SUN DRIED TOMATO & SPINACH",
                            "SUN DRIED TOMATO BASIL"), flavor_id := 7]

gardein[flavor_scent %in% c("BASIL PESTO",
                            "FRENCH HERB",
                            "GARDEN HERB",
                            "GARLIC & HERB",
                            "GRAIN & HERB",
                            "HERB & SPICE",
                            "HERB ROASTED",
                            "HERBY GARLIC GREENS",
                            "HICKORY & SAGE",
                            "HICKORY & SAGE SMOKED",
                            "HICKORY SMOKED",
                            "LEMON HERB",
                            "LEMON PEPPER",
                            "PARMESAN GARLIC",
                            "SEASON & LIME",
                            "SEASONED",
                            "SUN DRIED TOMATO & SPINACH",
                            "SUN DRIED TOMATO BASIL"), flavor_cat := "HERB"]


gardein[flavor_scent %in% c("BOMBAY CURRY",
                            "CHICK PEAS & CURRY",
                            "COCONUT CURRY",
                            "CURRIED SWEET POTATO",
                            "CURRY",
                            "INDIAN SPICED MASALA",
                            "MADRAS CURRY",
                            "MASALA",
                            "SPICEY INDIAN VEGETABLE",
                            "SPICY INDIAN",
                            "SWEET CURRY CARROT",
                            "TANDOORI SPICE"), flavor_id := 8]

gardein[flavor_scent %in% c("BOMBAY CURRY",
                            "CHICK PEAS & CURRY",
                            "COCONUT CURRY",
                            "CURRIED SWEET POTATO",
                            "CURRY",
                            "INDIAN SPICED MASALA",
                            "MADRAS CURRY",
                            "MASALA",
                            "SPICEY INDIAN VEGETABLE",
                            "SPICY INDIAN",
                            "SWEET CURRY CARROT",
                            "TANDOORI SPICE"), flavor_cat := "INDIAN"]


gardein[flavor_scent %in% c("CHICKEN SCALOPPINI",
                            "CHICAGO ITALIAN",
                            "CLASSIC PIZZERIA",
                            "HOT ITALIAN",
                            "ITALIAN",
                            "ITALIAN GARLIC & FENNEL",
                            "ITALIAN PEPPERONI",
                            "ITALIAN SAUSAGE",
                            "ITALIAN STYLE",
                            "KIELBASA",
                            "MAMA MIA SPICY ITALIAN",
                            "MILD ITALIAN",
                            "PARMIGIANA",
                            "PESTO MOZZARELLA",
                            "PIZZA PEPPERONI",
                            "SALAMI",
                            "SAVORY TUSCAN STYLE",
                            "SPICY ITALIAN",
                            "SPICY ITALIAN HEMPSEED",
                            "SWEET ITALIAN",
                            "TOMATO & BASIL PIZZA",
                            "TUSCAN VEGETABLE SAUSAGE",
                            "ZESTY CHICKEN",
                            "ZESTY ITALIAN"), flavor_id := 9]

gardein[flavor_scent %in% c("CHICKEN SCALOPPINI",
                            "CHICAGO ITALIAN",
                            "CLASSIC PIZZERIA",
                            "HOT ITALIAN",
                            "ITALIAN",
                            "ITALIAN GARLIC & FENNEL",
                            "ITALIAN PEPPERONI",
                            "ITALIAN SAUSAGE",
                            "ITALIAN STYLE",
                            "KIELBASA",
                            "MAMA MIA SPICY ITALIAN",
                            "MILD ITALIAN",
                            "PARMIGIANA",
                            "PESTO MOZZARELLA",
                            "PIZZA PEPPERONI",
                            "SALAMI",
                            "SAVORY TUSCAN STYLE",
                            "SPICY ITALIAN",
                            "SPICY ITALIAN HEMPSEED",
                            "SWEET ITALIAN",
                            "TOMATO & BASIL PIZZA",
                            "TUSCAN VEGETABLE SAUSAGE",
                            "ZESTY CHICKEN",
                            "ZESTY ITALIAN"), flavor_cat := "ITALIAN"]



gardein[flavor_scent %in% c("BLACK FOREST HAM",
                            "BOLOGNA",
                            "PHILLY STEAK"), flavor_id := 10]

gardein[flavor_scent %in% c("BLACK FOREST HAM",
                            "BOLOGNA",
                            "PHILLY STEAK"), flavor_cat := "LUNCHMEAT"]



gardein[flavor_scent %in% c("ADZUKI BEAN & SWEET POTATO",
                            "ALL AMERICAN",
                            "ALL AMERICAN VEGETABLE",
                            "BEER",
                            "BLACK RICE",
                            "CORNED BEEF",
                            "ELYSIAN BEER",
                            "FLAX",
                            "GARLIC",
                            "GREEK",
                            "GREEK MOUSSAKA",
                            "HOT DOG",
                            "KATSU",
                            "MEDITERRANEAN",
                            "MEDITERRANEAN CHICK PEAS",
                            "MOROCCAN",
                            "PERFECT",
                            "SONOMA",
                            "SPANISH SMOKED",
                            "SWEDISH",
                            "TRUFFLE",
                            "ZESTY RANCH"), flavor_id := 11]

gardein[flavor_scent %in% c("ADZUKI BEAN & SWEET POTATO",
                            "ALL AMERICAN",
                            "ALL AMERICAN VEGETABLE",
                            "BEER",
                            "BLACK RICE",
                            "CORNED BEEF",
                            "ELYSIAN BEER",
                            "FLAX",
                            "GARLIC",
                            "GREEK",
                            "GREEK MOUSSAKA",
                            "HOT DOG",
                            "KATSU",
                            "MEDITERRANEAN",
                            "MEDITERRANEAN CHICK PEAS",
                            "MOROCCAN",
                            "PERFECT",
                            "SONOMA",
                            "SPANISH SMOKED",
                            "SWEDISH",
                            "TRUFFLE",
                            "ZESTY RANCH"), flavor_cat := "OTHER"]




gardein[flavor_scent %in% c("CHICKEN",
                            "CHICKEN LIGHTLY SEASONED",
                            "HAM",
                            "HAM STYLE ROAST",
                            "BEEF",
                            "BRATWURST",
                            "BUTTER",
                            "BUTTERMILK",
                            "CALIFORNIA BURGER",
                            "CELEBRATION",
                            "CHAR GRILLED",
                            "CLASSIC",
                            "FLAME GRILLED",
                            "GRILLED",
                            "GROUND BEEF",
                            "HOMESTYLE",
                            "LIGHTLY SEASONED",
                            "MEAT LOVERS",
                            "NEW ENGLAND STYLE",
                            "NUTTY",
                            "ORIGINAL",
                            "ORIGINAL BEEFY",
                            "ORIGINAL BRAT",
                            "ORIGINAL SAUSAGE",
                            "ORIGINAL TURKEY",
                            "OVEN ROASTED",
                            "OVEN ROASTED TURKEY",
                            "REGULAR",
                            "ROAST",
                            "ROASTED TURKEY",
                            "SALISBURY STYLE",
                            "SALT & PEPPER",
                            "SAVORY",
                            "SAVORY CHICKEN",
                            "SEA SALT & PEPPER",
                            "SIGNATURE STADIUM DOG",
                            "SIMPLY SEASONED",
                            "SMOKED HAM",
                            "SMOKED SALT & PEPPER STEAK",
                            "SPINACH CHICKEN",
                            "STEAK",
                            "STEAKHOUSE STYLE",
                            "SUNDAY FUNDAY",
                            "SWEET & SAVORY",
                            "SWEET & SRIGINAL",
                            "SWEET & TANGY",
                            "TASTY",
                            "THE BIG FRY",
                            "THE CLASSIC",
                            "THE OG",
                            "THE STALLION",
                            "TRADITIONAL",
                            "TURKEY",
                            "TURKEY ROAST",
                            "ULTIMATE",
                            "ULTIMATE BLACK BEAN",
                            "UNSEASONED",
                            "VEGETABLE CHICKEN",
                            "VEGETABLE GRILLER ORIGINAL",
                            "VEGETABLE GRILLER PRIME",
                            "VEGETABLE LOVERS",
                            "VEGETABLE MEAT LOVER",
                            "VEGETABLE PORK",
                            "VEGETARIAN"), flavor_id := 12]

gardein[flavor_scent %in% c("CHICKEN",
                            "CHICKEN LIGHTLY SEASONED",
                            "HAM",
                            "HAM STYLE ROAST",
                            "BEEF",
                            "BRATWURST",
                            "BUTTER",
                            "BUTTERMILK",
                            "CALIFORNIA BURGER",
                            "CELEBRATION",
                            "CHAR GRILLED",
                            "CLASSIC",
                            "FLAME GRILLED",
                            "GRILLED",
                            "GROUND BEEF",
                            "HOMESTYLE",
                            "LIGHTLY SEASONED",
                            "MEAT LOVERS",
                            "NEW ENGLAND STYLE",
                            "NUTTY",
                            "ORIGINAL",
                            "ORIGINAL BEEFY",
                            "ORIGINAL BRAT",
                            "ORIGINAL SAUSAGE",
                            "ORIGINAL TURKEY",
                            "OVEN ROASTED",
                            "OVEN ROASTED TURKEY",
                            "REGULAR",
                            "ROAST",
                            "ROASTED TURKEY",
                            "SALISBURY STYLE",
                            "SALT & PEPPER",
                            "SAVORY",
                            "SAVORY CHICKEN",
                            "SEA SALT & PEPPER",
                            "SIGNATURE STADIUM DOG",
                            "SIMPLY SEASONED",
                            "SMOKED HAM",
                            "SMOKED SALT & PEPPER STEAK",
                            "SPINACH CHICKEN",
                            "STEAK",
                            "STEAKHOUSE STYLE",
                            "SUNDAY FUNDAY",
                            "SWEET & SAVORY",
                            "SWEET & SRIGINAL",
                            "SWEET & TANGY",
                            "TASTY",
                            "THE BIG FRY",
                            "THE CLASSIC",
                            "THE OG",
                            "THE STALLION",
                            "TRADITIONAL",
                            "TURKEY",
                            "TURKEY ROAST",
                            "ULTIMATE",
                            "ULTIMATE BLACK BEAN",
                            "UNSEASONED",
                            "VEGETABLE CHICKEN",
                            "VEGETABLE GRILLER ORIGINAL",
                            "VEGETABLE GRILLER PRIME",
                            "VEGETABLE LOVERS",
                            "VEGETABLE MEAT LOVER",
                            "VEGETABLE PORK",
                            "VEGETARIAN"), flavor_cat := "REGULAR"]


gardein[flavor_scent %in% c("BLACK PEPPER",
                            "BLACK PEPPER BEEF",
                            "BUFFALO",
                            "BUFFALO STYLE CAULIFLOWER",
                            "BUFFALO TEMPEH",
                            "CAJUN",
                            "CHICK PEAS & RED PEPPER",
                            "CHILI BEAN",
                            "CRACKED BLACK PEPPER",
                            "EXTREME",
                            "FEISTY",
                            "GREEN CHILE & PINTO BEAN",
                            "GREEN CHILE CHEDDAR",
                            "HOT & SPICY",
                            "HOT & SPICY SAUSAGE",
                            "JALAPENO",
                            "JALAPENO & CILANTRO",
                            "KICKIN",
                            "MANGO CHIPOTLE",
                            "MILD HOT",
                            "NASHVILLE HOT",
                            "NASHVILLE HOT CHICKEN",
                            "PEPPER SEASONING",
                            "PEPPER STEAK",
                            "PEPPERED",
                            "PINEAPPLE CHIPOTLE",
                            "PINTO HABANERO",
                            "SAUCY BUFFALO",
                            "SPICY",
                            "SPICY BUFFALO",
                            "SPICY CHICKEN",
                            "SPICY FALAFEL",
                            "SPICY GARLIC",
                            "SPICY GREEN CHILI",
                            "SPICY HABANERO CHICKEN",
                            "SPICY JALAPENO",
                            "SPICY MUSHROOM",
                            "SPICY SAUSAGE"), flavor_id := 13]

gardein[flavor_scent %in% c("BLACK PEPPER",
                            "BLACK PEPPER BEEF",
                            "BUFFALO",
                            "BUFFALO STYLE CAULIFLOWER",
                            "BUFFALO TEMPEH",
                            "CAJUN",
                            "CHICK PEAS & RED PEPPER",
                            "CHILI BEAN",
                            "CRACKED BLACK PEPPER",
                            "EXTREME",
                            "FEISTY",
                            "GREEN CHILE & PINTO BEAN",
                            "GREEN CHILE CHEDDAR",
                            "HOT & SPICY",
                            "HOT & SPICY SAUSAGE",
                            "JALAPENO",
                            "JALAPENO & CILANTRO",
                            "KICKIN",
                            "MANGO CHIPOTLE",
                            "MILD HOT",
                            "NASHVILLE HOT",
                            "NASHVILLE HOT CHICKEN",
                            "PEPPER SEASONING",
                            "PEPPER STEAK",
                            "PEPPERED",
                            "PINEAPPLE CHIPOTLE",
                            "PINTO HABANERO",
                            "SAUCY BUFFALO",
                            "SPICY",
                            "SPICY BUFFALO",
                            "SPICY CHICKEN",
                            "SPICY FALAFEL",
                            "SPICY GARLIC",
                            "SPICY GREEN CHILI",
                            "SPICY HABANERO CHICKEN",
                            "SPICY JALAPENO",
                            "SPICY MUSHROOM",
                            "SPICY SAUSAGE"), flavor_cat := "SPICY"]


gardein[flavor_scent %in% c("FIESTA BLACK BEAN",
                            "BIG TEX",
                            "CHIPOTLE",
                            "CHIPOTLE BLACK BEAN",
                            "CHIPOTLE LIME",
                            "CHORIZO",
                            "COWGIRL",
                            "MEXICAN",
                            "MEXICAN CHIPOTLE",
                            "POBLANO BLACK BEAN",
                            "SANTA FE",
                            "SMOKED SALSA CHIPOTLE",
                            "SOUTH WEST",
                            "SOUTHWEST ADZUKI BEAN",
                            "SOUTHWEST BLACK BEAN",
                            "SOUTHWEST STYLE BEET",
                            "SOUTHWESTERN",
                            "SPICY BLACK BEAN",
                            "SPICY CHIPOTLE BLACK BEAN",
                            "TACO",
                            "TEX MEX",
                            "ZESTY MEXICAN"), flavor_id := 14]

gardein[flavor_scent %in% c("FIESTA BLACK BEAN",
                            "BIG TEX",
                            "CHIPOTLE",
                            "CHIPOTLE BLACK BEAN",
                            "CHIPOTLE LIME",
                            "CHORIZO",
                            "COWGIRL",
                            "MEXICAN",
                            "MEXICAN CHIPOTLE",
                            "POBLANO BLACK BEAN",
                            "SANTA FE",
                            "SMOKED SALSA CHIPOTLE",
                            "SOUTH WEST",
                            "SOUTHWEST ADZUKI BEAN",
                            "SOUTHWEST BLACK BEAN",
                            "SOUTHWEST STYLE BEET",
                            "SOUTHWESTERN",
                            "SPICY BLACK BEAN",
                            "SPICY CHIPOTLE BLACK BEAN",
                            "TACO",
                            "TEX MEX",
                            "ZESTY MEXICAN"), flavor_cat := "TEXMEX"]

gardein[flavor_scent %in% c("ARTICHOKE",
                            "BEAN",
                            "BEET",
                            "BEET & KALE",
                            "BEETROOT & BEAN",
                            "BLACK BEAN",
                            "BLACK BEAN & PINEAPPLE",
                            "BLACK BEAN & PLANTAIN",
                            "BLACK BEAN & VEGETABLE",
                            "BLACK BEAN CHIPOTLE",
                            "BLACK BEAN QUINOA",
                            "BROCCOLI BOOST",
                            "BROWN RICE & GARBANZO & WHITE BEAN",
                            "BUBBA",
                            "CALIFORNIA VEGETABLE",
                            "CARROT",
                            "CAULIFLOWER",
                            "CHICK PEAS & SUNFLOWER SEED",
                            "CHICK PEAS & TAHINI",
                            "CHICK PEAS SWEET POTATO",
                            "EGGPLANT",
                            "EL CAPITAN",
                            "EL GUAPO",
                            "EL ZAPATISTA",
                            "FALAFEL",
                            "FALAFEL & SESAME",
                            "GARDEN VARIETY",
                            "GARDEN VEGETABLE",
                            "GREEN & BEAN MEDLEY",
                            "GRILLED VEGETABLE",
                            "HARVEST BLEND",
                            "HAZELNUT CRANBERRY",
                            "HEIRLOOM BEAN",
                            "HEMPSEED",
                            "KALE",
                            "KALE & QUINOA",
                            "MULTI VEGETABLE",
                            "MUSHROOM",
                            "MUSHROOM & CHEESE",
                            "MUSHROOM & VEGETABLE",
                            "MUSHROOM & WINE",
                            "MUSHROOM MISO",
                            "MUSHROOM QUINOA",
                            "MUSHROOM RICE",
                            "MUSHROOM RISOTTO",
                            "PEAS & CARROT",
                            "PEPPADEW",
                            "PEPPADEW PEPPER",
                            "PEPPADEW PIQUANTE PEPPER",
                            "PORTABELLO",
                            "PORTABELLO MUSHROOM & CHEESE",
                            "PORTABELLO QUINOA",
                            "PUMPKIN & SPINACH",
                            "ROASTED BEET & KALE",
                            "ROOT VEGETABLE",
                            "SAVORY MUSHROOM",
                            "SAVORY MUSHROOM & ROASTED GARLIC",
                            "SAVORY ORANGE",
                            "SAVORY ORIGINAL",
                            "SAVORY VEGETABLE",
                            "SAVORY VEGETABLE SAUSAGE",
                            "SHIITAKE MUSHROOM",
                            "SMOKED TOMATO",
                            "SPINACH",
                            "SPINACH PESTO",
                            "SUMMER HARVEST",
                            "SUNFLOWER BEET",
                            "SUPER CAULIFLOWER",
                            "SUPER GREENS",
                            "SWEET HEAT BEET",
                            "SWEET PEPPER",
                            "SWEET POTATO",
                            "SWEET POTATO & VEGETABLE",
                            "SWEET POTATO SUNFLOWER",
                            "SWEET SUNSHINE CORN",
                            "TOMATO & SPINACH",
                            "TUSCAN KALE WHITE BEAN",
                            "VEGAN",
                            "VEGETABLE",
                            "WHITE BEAN & KALE",
                            "WHITE BEAN CHILI",
                            "WHITE TRUFFLE & BLACK BEAN",
                            "WILD MUSHROOM",
                            "WILD MUSHROOM CAULIFLOWER HEMPSEED"), flavor_id := 15]

gardein[flavor_scent %in% c("ARTICHOKE",
                            "BEAN",
                            "BEET",
                            "BEET & KALE",
                            "BEETROOT & BEAN",
                            "BLACK BEAN",
                            "BLACK BEAN & PINEAPPLE",
                            "BLACK BEAN & PLANTAIN",
                            "BLACK BEAN & VEGETABLE",
                            "BLACK BEAN CHIPOTLE",
                            "BLACK BEAN QUINOA",
                            "BROCCOLI BOOST",
                            "BROWN RICE & GARBANZO & WHITE BEAN",
                            "BUBBA",
                            "CALIFORNIA VEGETABLE",
                            "CARROT",
                            "CAULIFLOWER",
                            "CHICK PEAS & SUNFLOWER SEED",
                            "CHICK PEAS & TAHINI",
                            "CHICK PEAS SWEET POTATO",
                            "EGGPLANT",
                            "EL CAPITAN",
                            "EL GUAPO",
                            "EL ZAPATISTA",
                            "FALAFEL",
                            "FALAFEL & SESAME",
                            "GARDEN VARIETY",
                            "GARDEN VEGETABLE",
                            "GREEN & BEAN MEDLEY",
                            "GRILLED VEGETABLE",
                            "HARVEST BLEND",
                            "HAZELNUT CRANBERRY",
                            "HEIRLOOM BEAN",
                            "HEMPSEED",
                            "KALE",
                            "KALE & QUINOA",
                            "MULTI VEGETABLE",
                            "MUSHROOM",
                            "MUSHROOM & CHEESE",
                            "MUSHROOM & VEGETABLE",
                            "MUSHROOM & WINE",
                            "MUSHROOM MISO",
                            "MUSHROOM QUINOA",
                            "MUSHROOM RICE",
                            "MUSHROOM RISOTTO",
                            "PEAS & CARROT",
                            "PEPPADEW",
                            "PEPPADEW PEPPER",
                            "PEPPADEW PIQUANTE PEPPER",
                            "PORTABELLO",
                            "PORTABELLO MUSHROOM & CHEESE",
                            "PORTABELLO QUINOA",
                            "PUMPKIN & SPINACH",
                            "ROASTED BEET & KALE",
                            "ROOT VEGETABLE",
                            "SAVORY MUSHROOM",
                            "SAVORY MUSHROOM & ROASTED GARLIC",
                            "SAVORY ORANGE",
                            "SAVORY ORIGINAL",
                            "SAVORY VEGETABLE",
                            "SAVORY VEGETABLE SAUSAGE",
                            "SHIITAKE MUSHROOM",
                            "SMOKED TOMATO",
                            "SPINACH",
                            "SPINACH PESTO",
                            "SUMMER HARVEST",
                            "SUNFLOWER BEET",
                            "SUPER CAULIFLOWER",
                            "SUPER GREENS",
                            "SWEET HEAT BEET",
                            "SWEET PEPPER",
                            "SWEET POTATO",
                            "SWEET POTATO & VEGETABLE",
                            "SWEET POTATO SUNFLOWER",
                            "SWEET SUNSHINE CORN",
                            "TOMATO & SPINACH",
                            "TUSCAN KALE WHITE BEAN",
                            "VEGAN",
                            "VEGETABLE",
                            "WHITE BEAN & KALE",
                            "WHITE BEAN CHILI",
                            "WHITE TRUFFLE & BLACK BEAN",
                            "WILD MUSHROOM",
                            "WILD MUSHROOM CAULIFLOWER HEMPSEED"), flavor_cat := "VEGETABLE"]


gardein$MonthYear <- format(as.Date(gardein$time),"%m-%Y")
gardein$Year <- format(as.Date(gardein$time),"%Y")
gardein$Month <- format(as.Date(gardein$time),"%m")


gardein <- gardein %>%
  select(-"product_type.id")

#adding seasons based on months and enumerating them
metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter"
)
metseasonsNum <- c(
  "Winter" = "0",
  "Spring" = "1",
  "Summer" = "2",
  "Fall" = "3"
)

gardein$Season <- metseasons[gardein$Month]
gardein$SeasonNum <- metseasonsNum[gardein$Season]


#Dummy Variables for Promos
gardein$ppuAnyMerch <- ifelse(is.na(gardein$price_per_unit_any_merch) == TRUE, 0, 1)  
gardein$ppuNoMerch <- ifelse(is.na(gardein$price_per_unit_no_merch) == TRUE, 0, 1)
gardein$ppuUnitRedOnly <- ifelse(is.na(gardein$price_per_unit_price_reductions_only) == TRUE, 0, 1)
gardein$ppuFeatOnly <- ifelse(is.na(gardein$price_per_unit_feature_only) == TRUE, 0, 1)
gardein$ppuDispOnly <- ifelse(is.na(gardein$price_per_unit_display_only) == TRUE, 0, 1)
gardein$ppuSpecPackOnly <- ifelse(is.na(gardein$price_per_unit_special_pack_only) == TRUE, 0, 1)
gardein$ppuFeatNDisp <- ifelse(is.na(gardein$price_per_unit_feature_and_display) == TRUE, 0, 1)

#Creating Dummy Variables for Region
gardein$calDum <- ifelse(gardein$geography == "California", 1, 0)
gardein$plainDum <- ifelse(gardein$geography == "Plains", 1, 0)
gardein$seDum <- ifelse(gardein$geography == "Southeast", 1, 0)
gardein$neDum <- ifelse(gardein$geography == "Northeast", 1, 0)
gardein$glDum <- ifelse(gardein$geography == "GreatLakes ", 1, 0)
gardein$scDum <- ifelse(gardein$geography == "SouthCentral ", 1, 0)
gardein$msDum <- ifelse(gardein$geography == "MidSouth", 1, 0)
gardein$westDum <- ifelse(gardein$geography == "West", 1, 0)
gardein$totDum <- ifelse(gardein$geography == "TotalUS", 1,0)

#dummies for productType categories
gardein$poulDum <- ifelse(gardein$product_type_cat == "POULTRY", 1,0)
gardein$nonMeatDum <- ifelse(gardein$product_type_cat == "NON-MEAT", 1,0)
gardein$seaDum <- ifelse(gardein$product_type_cat == "SEAFOOD", 1,0)
gardein$meatDum <- ifelse(gardein$product_type_cat == "MEAT", 1,0)

#dummies for form cat
gardein$filetDum <- ifelse(gardein$form_cat == "FILET/STRIP", 1,0)
gardein$grndDum <- ifelse(gardein$form_cat == "GROUND", 1,0)
gardein$linkDum <- ifelse(gardein$form_cat == "LINK", 1,0)
gardein$meatballDum <- ifelse(gardein$form_cat == "MEATBALL", 1,0)
gardein$nuggetDum <- ifelse(gardein$form_cat == "NUGGET / TENDER", 1,0)
gardein$otherFormDum <- ifelse(gardein$form_cat == "OTHER", 1,0)
gardein$pattyDum <- ifelse(gardein$form_cat == "PATTY / BURGER", 1,0)
gardein$slicedDum <- ifelse(gardein$form_cat == "SLICED", 1,0)

#flavor dummies
gardein$flvAsianDum <- ifelse(gardein$flavor_cat == "ASIAN", 1,0)
gardein$flBbqDum <- ifelse(gardein$flavor_cat == "BARBEQUE", 1,0)
gardein$flvBfstDum <- ifelse(gardein$flavor_cat == "BREAKFAST", 1,0)
gardein$flvChseDum <- ifelse(gardein$flavor_cat == "CHEESE", 1,0)
gardein$flvFishnDum <- ifelse(gardein$flavor_cat == "FISH", 1,0)
gardein$flvFreshnDum <- ifelse(gardein$flavor_cat == "FRESH", 1,0)
gardein$flvGrainDum <- ifelse(gardein$flavor_cat == "GRAIN", 1,0)
gardein$flvHerbDum <- ifelse(gardein$flavor_cat == "HERB", 1,0)
gardein$flvIndnDum <- ifelse(gardein$flavor_cat == "INDIAN", 1,0)
gardein$flvItlnDum <- ifelse(gardein$flavor_cat == "ITALIAN", 1,0)
gardein$flvLnchMtnDum <- ifelse(gardein$flavor_cat == "LUNCHMEAT", 1,0)
gardein$flvOthrDum <- ifelse(gardein$flavor_cat == "OTHER", 1,0)
gardein$flvRglrDum <- ifelse(gardein$flavor_cat == "REGULAR", 1,0)
gardein$flvSpicyDum <- ifelse(gardein$flavor_cat == "SPICY", 1,0)
gardein$flvTxMxDum <- ifelse(gardein$flavor_cat == "TEXMEX", 1,0)
gardein$flvVgtlDum <- ifelse(gardein$flavor_cat == "VEGETABLE", 1,0)

#TypeofSub Dummies
gardein$tosUnkDum <- ifelse(gardein$type_of_meat_substituted_cat == "UNKNOWN", 1,0)
gardein$tosBeefDum <- ifelse(gardein$type_of_meat_substituted_cat == "BEEF", 1,0)
gardein$tosBurgDum <- ifelse(gardein$type_of_meat_substituted_cat == "BURGER", 1,0)
gardein$tosChkDum <- ifelse(gardein$type_of_meat_substituted_cat == "CHICKEN", 1,0)
gardein$tosMbDum <- ifelse(gardein$type_of_meat_substituted_cat == "MEATBALL", 1,0)
gardein$tosNnMtDum <- ifelse(gardein$type_of_meat_substituted_cat == "NON-MEAT", 1,0)
gardein$tosPrkDum <- ifelse(gardein$type_of_meat_substituted_cat == "PORK", 1,0)
gardein$tosSnwchDum <- ifelse(gardein$type_of_meat_substituted_cat == "SANDWICH", 1,0)
gardein$tosSsgDUm <- ifelse(gardein$type_of_meat_substituted_cat == "SAUSAGE", 1,0)
gardein$tosSfDum <- ifelse(gardein$type_of_meat_substituted_cat == "SEAFOOD", 1,0)

gardein$y20 <- ifelse(gardein$Year == 2020, 1, 0)
gardein$y21 <- ifelse(gardein$Year == 2021, 1, 0)
gardein$y22 <- ifelse(gardein$Year == 2022, 1, 0)
gardein$y23 <- ifelse(gardein$Year == 2023, 1, 0)
gardein$y24 <- ifelse(gardein$Year == 2024, 1, 0)

#export Dataset to a CSV
write.csv(subsDf, "/Users/saipyneni/Documents/Conagra Project/Conagra Project/Data/subsMeatCleaned.csv", row.names=FALSE)
write.csv(gardein, "/Users/saipyneni/Documents/Conagra Project/Conagra Project/Data/gardeinCleaned.csv", row.names=FALSE)




#--------END OF DATA CLEANING-------#
#--------Below is extra code -------#

#Use this code to encode the categorical data into numbner format
library(data.table)
setDT(gardein)
gardein[, geography.id := .GRP, geography]
gardein[, form.id := .GRP, form]
gardein[, sub_category_name.id := .GRP, sub_category_name]
gardein[, package.id := .GRP, package]
gardein[, product_type.id := .GRP, product_type]
gardein[, meat_source.id := .GRP, meat_source]
gardein[, type_of_meat_substituted.id := .GRP, type_of_meat_substituted]
gardein[, type_of_substitute.id := .GRP, type_of_substitute]
gardein[, cooked_info.id := .GRP, cooked_info]
gardein[, Year.id := .GRP, Year]
gardein[, unitAnySale.id := .GRP, unit_sales_any_merch]

#import Dataset to work
subsDf <- read.csv("subsMeatCleaned.csv")


#average sales per brand
subsYrAgg <- setNames(aggregate(subsDf$unit_sales, list(subsDf$brand_name), FUN = mean), c("brand", "averageSales"))


#aggregate mean unit sales by brand
subAgg <- setNames(aggregate(subsDf$`Unit Sales`, list(subsDf$`Brand Name`, 
                                                       format(as.Date(subsDf$Time, format="%d/%m/%Y"),"%Y-m")), FUN=mean), 
                   c("brand", "monthYear", "meanUnitSales" ))



