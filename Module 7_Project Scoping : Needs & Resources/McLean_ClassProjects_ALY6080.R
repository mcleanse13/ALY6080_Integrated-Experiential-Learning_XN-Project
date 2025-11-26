install.packages("tidyverse")
library(tidyverse)
yeoman <- read.xlsx("Northeastern_Master_US_Search_Query_Performance_Brand_View_Comprehensive_Quarter_2024_03_31.xlsx")
View(yeoman)
head(yeoman)
summary(yeoman)
str(yeoman)

# Load necessary libraries
library(MASS)  # for lda
library(caret) # for train-test split and accuracy calculation
library(dplyr) # for data manipulation

# Scatter plot
plot(yeoman$`Cart Adds: Cart Add Rate %`, yeoman$`Purchases: Purchase Rate %`, main = "Scatter Plot of X vs Y", xlab = "X", ylab = "Y", col = "blue", pch = 1)

# Histogram
hist(yeoman$`Clicks: Brand Share %`, main = "Clicks: Brand Share %", xlab = "Percentage", ylab = "Search Query #", col = "red")
