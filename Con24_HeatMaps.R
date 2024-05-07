rm(list=ls())

library(tidyverse)
install.packages("tidyverse")
library(corrplot)
install.packages("corrplot")
library(dplyr)
library(tidyr)



Gardein <- gardeinCleaned_6
Gardein_flavor <- Gardein %>%
  select(flavor_cat, unit_sales)

#all_flavors <- c("7 GRAIN", "BEEF", "CHICKEN", "CHICKEN SCALOPPINI", "CHIPOTLE BLACK BEAN",
#                 "CHIPOTLE LIME", "CLASSIC", "FISH", "GARDEN", "HOMESTYLE", "ITALIAN",
#                 "ITALIAN SAUSAGE", "MANDARIN ORANGE", "MAPLE SAUSAGE", "NASHVILLE HOT",
#                 "ORIGINAL", "REGULAR", "ROAST", "SAVORY", "SIZZLING SZECHUAN",
#                 "SPICY", "SWEET & SOUR", "TERIYAKI", "TURKEY", "ULTIMATE", "ULTIMATE BLACK BEAN")

#Gardein_flavor$flavor_cat <- factor(Gardein_flavor$flavor_cat, levels = all_flavors)

dummy_flavors <- model.matrix(~ 0 + Gardein_flavor$flavor_cat)

#dummy_flavors_filtered <- dummy_flavors[, apply(dummy_flavors, 2, sd) != 0]

correlation_matrix_flavor <- cor(dummy_flavors)

flavor_names <- gsub("Gardein_flavor\\$flavor_cat", "", colnames(correlation_matrix_flavor))
print(flavor_names)
colnames(correlation_matrix_flavor) <- flavor_names
rownames(correlation_matrix_flavor) <- flavor_names

#correlation_matrix_2 <- cor(dummy_flavors_filtered)

legend_values <- c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)

heatmap(correlation_matrix_flavor,
        symm = TRUE,
        Rowv = NA,
        Colv = NA,
        col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Flavor Correlation Heatmap")

legend("bottomright", legend = legend_values,
       fill = colorRampPalette(c("blue", "white", "red"))(length(legend_values)),
       title = "Correlation",
       cex = 0.8)


Gardein_form <- Gardein %>%
  select(form_cat, unit_sales)

dummy_form <- model.matrix(~ 0 + Gardein_form$form_cat)

correlation_matrix_form <- cor(dummy_form)

form_names <- gsub("Gardein_form\\$form_cat", "", colnames(correlation_matrix_form))
print(form_names)
colnames(correlation_matrix_form) <- form_names
rownames(correlation_matrix_form) <- form_names

legend_values <- c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)

heatmap(correlation_matrix_form,
        symm = TRUE,
        Rowv = NA,
        Colv = NA,
        col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Form Correlation Heatmap")

legend("bottomright", legend = legend_values,
       fill = colorRampPalette(c("blue", "white", "red"))(length(legend_values)),
       title = "Correlation",
       cex = 0.8)


Gardein_toms <- Gardein %>%
  select(type_of_meat_substituted_cat, unit_sales)

dummy_toms <- model.matrix(~ 0 + Gardein_toms$type_of_meat_substituted_cat)

correlation_matrix_toms <- cor(dummy_toms)

toms_names <- gsub("Gardein_toms\\$type_of_meat_substituted_cat", "", colnames(correlation_matrix_toms))
print(toms_names)
colnames(correlation_matrix_toms) <- toms_names
rownames(correlation_matrix_toms) <- toms_names

legend_values <- c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)

heatmap(correlation_matrix_toms,
        symm = TRUE,
        Rowv = NA,
        Colv = NA,
        col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Type of Meat Substitute Correlation Heatmap")

legend("bottomright", legend = legend_values,
       fill = colorRampPalette(c("blue", "white", "red"))(length(legend_values)),
       title = "Correlation",
       cex = 0.8)














































#Heatmap for substitutes