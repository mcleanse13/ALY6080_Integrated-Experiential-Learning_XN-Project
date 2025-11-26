# Install the readxl package if you haven't already
install.packages("readxl")
install.packages(c("tidyverse", "skimr"))

# Load the readxl package
library(readxl)
# Load necessary libraries
library(MASS)  # for lda
library(caret) # for train-test split and accuracy calculation
library(dplyr) # for data manipulation
library(tidyverse)
library(lubridate)
library(skimr)
library(ggplot2)

# Read the Excel file
yeoman <- read_excel("Northeastern_Master_US_Search_Query_Performance_Brand_View_Comprehensive_Quarter_2024_03_31.xlsx")
View(yeoman)
head(yeoman)
summary(yeoman)
str(yeoman)
dim(yeoman)
skim(yeoman)
glimpse(yeoman)

# Check for missing values
sapply(yeoman, function(x) sum(is.na(x)))

# Scatter plot
plot(yeoman$`Cart Adds: Cart Add Rate %`, yeoman$`Purchases: Purchase Rate %`, main = "Scatter Plot of X vs Y", xlab = "X", ylab = "Y", col = "blue", pch = 1)

# Histogram
hist(yeoman$`Clicks: Brand Share %`, main = "Clicks: Brand Share %", xlab = "Percentage", ylab = "Search Query #", col = "red")

# Data Preprocessing
# Handle missing values
# Check for missing values
colSums(is.na(yeoman))

# Identify non-numeric values
non_numeric_values <- yeoman$`Clicks: Total Count`[is.na(as.numeric(as.character(data$numeric_variable)))]
print(non_numeric_values)

# Exploratory Data Analysis (EDA)
# Histogram for a specific numeric variable
ggplot(yeoman, aes(x = `Purchases: Brand Share %`)) +
  geom_histogram(binwidth = 30) +
  labs(title = "Histogram of Purchases: Brand Share %", x = "Percentage", y = "Purchases: Brand Share")

numeric_vars <- yeoman %>% select_if(is.numeric)

for (var in names(numeric_vars)) {
  print(
    ggplot(yeoman, aes(x = !!sym(var))) +
      geom_histogram(binwidth = 30, fill = "blue", color = "black") +
      labs(title = paste("Histogram of", var), x = var, y = "Count")
  )
}

# Bar plot for a specific categorical variable
ggplot(yeoman, aes(x = `Brand Name`)) +
  geom_bar() +
  labs(title = "Bar Plot of Brand Names", x = "Brand Names", y = "Count")

install.packages("naniar")
library(naniar)

# Visualize missing data
gg_miss_var(yeoman)

# Calculate correlation matrix
cor_matrix <- cor(select_if(yeoman, is.numeric), use = "complete.obs")

# Visualize the correlation matrix
library(ggcorrplot)
ggcorrplot::ggcorrplot(cor_matrix, lab = TRUE)

# Scatter plot
ggplot(yeoman, aes(x = `Impressions: Total Count`, y = `Clicks: Total Count`)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Numeric Variable1 vs Numeric Variable2", x = "Numeric Variable1", y = "Numeric Variable2")

# Scatter plot
ggplot(yeoman, aes(x = `Clicks: Total Count`, y = `Purchases: Total Count`)) +
  geom_point(alpha = 0.5) +
  labs(title = "'Clicks: Total Count' vs 'Purchases: Total Count'", x = "Clicks: Total Count", y = "Purchases: Total Count")

# Summary statistics by a categorical variable
library(readr)
summary_stats <- yeoman %>%
  group_by(`Brand Name`) %>%
  summarize(across(where(is.numeric), list(mean = ~mean(.x, na.rm = TRUE),
                                           sd = ~sd(.x, na.rm = TRUE),
                                           median = ~median(.x, na.rm = TRUE))))
summary_stats

#Association Mining - Sean McLean
#load libraries and data
install.packages("arulesViz")
library(arulesViz)
require(arules)

# Read your dataset into a transaction object 
#ChatGpt reference from lines 11 to 25
# Replace "your_dataset.csv" with the filename of your dataset
amazon <- read.transactions("Northeastern_Master_US_Search_Query_Performance_Brand_View_Comprehensive_Quarter_2024_03_31.xlsx", format = "basket", sep = ",", rm.duplicates = TRUE)

# Inspect the transaction object
summary(movies)

# Perform association rule mining
rules <- apriori(yeoman, parameter = list(support = 0.08, confidence = 0.4))

# Inspect the discovered rules
summary(rules)
inspect(rules)
plot(rules)

# number of items in each observation
size(head(amazon))
LIST(head(amazon, 3))

# calculates support for frequent items
frequentItems <- eclat (amazon, parameter = list(supp = 0.07, 
                                                 maxlen = 15))
inspect(frequentItems)

# plot frequent items
itemFrequencyPlot(amazon, topN=10, type="absolute", 
                  main="Item Frequency")

# Min Support as 0.01, confidence as 0.8.
rules <- apriori (amazon, parameter = list(supp = 0.01, 
                                           conf = 0.8))
# 'high-confidence' rules.
rules_conf <- sort (rules, by="confidence", decreasing=TRUE)
# show the support, lift and confidence for all rules
inspect(head(rules_conf)) 

#rules with highest lift
rules_lift <- sort (rules, by="lift", decreasing=TRUE)
# show the support, lift and confidence for all rules
inspect(head(rules_lift)) 

# maxlen = 3 limits the elements in a rule to 3
rules <- apriori(amazon, parameter = list (supp = 0.001, 
                                           conf = 0.5, 
                                           maxlen=3))

#remove subset rules
length(rules)
subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
length(subsetRules)
rules <- rules[-subsetRules] # remove subset rules. 
length(rules)

# get rules that watched a production with a "TV-MA" rating
rules <- apriori (data=amazon, 
                  parameter=list (supp=0.001,conf = 0.05), 
                  appearance = list (default="lhs",rhs="Brand Name"), 
                  control = list (verbose=F))

# 'high-confidence' rules.
rules_conf <- sort (rules, by="lift", decreasing=TRUE)
inspect(head(rules_conf))

# those who watched a production with a "TV-MA" rating also watched...
rules <- apriori (data=amazon, parameter=list (supp=0.001,
                                               conf = 0.15,
                                               minlen=2), 
                  appearance = list(default="rhs",lhs="TV-MA"), 
                  control = list (verbose=F)) 

# 'high-confidence' rules
rules_conf <- sort (rules, by="confidence", decreasing=TRUE)
inspect(head(rules_conf))

#visualize rules
library(arules)
library(arulesViz)
library(RColorBrewer)

rules <- apriori(amazon, parameter = list(supp = 0.01, conf = 0.2))

inspect(rules[1:10])

arules::itemFrequencyPlot(amazon, topN = 20, 
                          col = brewer.pal(8, 'Pastel2'),
                          main = 'Relative Item Frequency Plot',
                          type = "relative",
                          ylab = "Item Frequency (Relative)")

plot(rules, measure = c("support", "lift"), shading = "confidence")

plot(rules, method = "grouped")

#Clustering
library(dplyr)
library(readr)
library(ggplot2)
colnames(yeoman)
# Select relevant variables for clustering
clustering_vars <- yeoman %>%
  select(`Search Query Score`,
         `Impressions: Total Count`,
         `Clicks: Total Count`,
         `Cart Adds: Total Count`,
         `Purchases: Total Count`,
         `Clicks: Click Rate %`,
         `Purchases: Purchase Rate %`,
         `Impressions: Brand Share %`,
         `Clicks: Brand Share %`,
         `Purchases: Brand Share %`,
         `Clicks: Price (Median)`,
         `Cart Adds: Price (Median)`,
         `Purchases: Price (Median)`)

# Standardize the variables (z-score normalization)
clustering_vars_scaled <- scale(clustering_vars)

