install.packages("tidyverse")
install.packages("learnr")
install.packages("readxl")
install.packages("readr")
installed.packages("dplyr")
installed.packages("ggplot2")
install.packages("corrplot")
#rm(list=ls()) # clear the environment


library(learnr)
library(dplyr)
library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(corrplot)
setwd("C:/Users/HP/Documents/TOLU_UK_ASSIGNMENT")
# Assuming 'df' is the data frame containing the dataset


dataset <- read_excel("~/TOLU_UK_ASSIGNMENT/DATAS.xlsx")
View(dataset)


# Data preparation
# Assuming you want to deal with missing values by removing rows with missing values
dataset <- dataset %>% drop_na()


# Outlier detection using boxplots
# Assuming you want to check for outliers in the 'Stocks traded' column
ggplot(dataset, aes(x = 1, y = `Stockstraded`)) +
  geom_boxplot() +
  ggtitle("Boxplot of Stocks Traded")


# Scatter plot: Relationship between 'Stocks traded' and 'Market Capitalization'
ggplot(dataset, aes(x = `Stockstraded`, y = MarketC)) +
  geom_point() +
  ggtitle("Relationship between Stocks Traded and Market Capitalization") +
  xlab("Stocks Traded") +
  ylab("Market Capitalization")


# Pair plot for multiple variables
# Note: This might take a bit longer for larger datasets
pairs(dataset[, sapply(dataset, is.numeric)])






install.packages("moments")
library(moments)

# Descriptive Statistics
summary_stats <- summary(dataset)
view(summary_stats)



# Select numeric columns
numeric_cols <- sapply(dataset, is.numeric)
numeric_dataset <- dataset[, numeric_cols]

# Descriptive Statistics
mean_values <- colMeans(numeric_dataset, na.rm = TRUE)
median_values <- sapply(numeric_dataset, median, na.rm = TRUE)

# Mode calculation using a custom function
mode_values <- apply(numeric_dataset, 2, function(x) {
  tbl <- table(x)
  as.numeric(names(tbl)[which.max(tbl)])
})

# Combine the results into a data frame
descriptive_stats <- data.frame(
  Variable = colnames(numeric_dataset),
  Mean = mean_values,
  Median = median_values,
  Mode = mode_values
)

# Print the results
print(descriptive_stats)




# Standard Deviation
sd_values <- apply(dataset, 2, sd, na.rm = TRUE)
view(sd_values)

# Skewness calculation
skewness_values <- apply(dataset, 2, function(x) {
  if (is.numeric(x)) {
    skewness(x, na.rm = TRUE)
    print(skewness_values)
  } else {
    NA
  }
})

# Kurtosis calculation
# Kurtosis calculation
kurtosis_values <- apply(df, 2, function(x) {
  if (is.numeric(x)) {
    result <- kurtosis(x, na.rm = TRUE)
    print(result)
    return(result)
  } else {
    print(NA)
    return(NA)
  }
})



# Load necessary libraries
library(dplyr)

# Select only numeric columns
numeric_dataset <- select_if(dataset, is.numeric)

# Calculate correlation matrix
correlation_matrix <- cor(numeric_dataset, use = "complete.obs")

# Print correlation matrix
print(correlation_matrix)



# Objective 1: Temporal Analysis of Economic Indicators 
# Create a character vector


# Convert the character vector to numeric


# Print the result
print(numeric_vector)

cleaned_data <- na.omit(dataset)


# Objective 1: Temporal Analysis of Economic Indicators
objective_1_indicators <- cleaned_data %>% 
  select(Stockstraded, MarketC, Inflation)

# Convert selected columns to numeric
objective_1_indicators <- lapply(objective_1_indicators, as.numeric)

# Create a new data frame with the numeric columns
objective_1_numeric <- as.data.frame(objective_1_indicators)

# Check the structure of the new data frame
str(objective_1_numeric)



objective_1_indicators <- cleaned_data %>%
  select("Stockstraded", "MarketC", "Inflation")
objective_1_indicators <- as.numeric(objective_1_indicators)

# Calculate correlation matrix for Objective 1 indicators
cor_matrix_obj_1 <- cor(objective_1_numeric, use = "complete.obs")

# Print correlation matrix
print("Correlation Matrix - Objective 1")
print(cor_matrix_obj_1)

# Visualize correlation matrix
corrplot(cor_matrix_obj_1, method = "color", type = "upper", tl.col = "black", tl.srt = 45)




# Objective 2: Impact of Interest Rates on Economic Performance
objective_2_indicators <- cleaned_data %>%
  select("Dinterest", "IR", "turnoveR", "exchangeR")

# Convert selected columns to numeric
objective_2_indicators <- lapply(objective_2_indicators, as.numeric)

# Create a new data frame with the numeric columns
objective_2_numeric <- as.data.frame(objective_2_indicators)

# Check the structure of the new data frame
str(objective_2_numeric)



objective_2_indicators <- cleaned_data %>%
  select("Dinterest", "IR", "turnoveR", "exchangeR")
objective_1_indicators <- as.numeric(objective_2_indicators)

# Calculate correlation matrix for Objective 1 indicators
cor_matrix_obj_2 <- cor(objective_2_numeric, use = "complete.obs")

# Print correlation matrix
print("Correlation Matrix - Objective 1")
print(cor_matrix_obj_2)

# Visualize correlation matrix
corrplot(cor_matrix_obj_2, method = "color", type = "upper", tl.col = "black", tl.srt = 45)



# "Stocks traded", "Market capitalization", "Inflation", "Deposit interest rates", "Lending interest rates", "Real economic growth


# Objective 3: Cross-Country Disparities and Convergence
objective_3_indicators <- cleaned_data %>%
  select("Dinterest", "IR", "turnoveR", "MarketC", "Stockstraded", "Inflation", "turnoveR")

# Convert selected columns to numeric
objective_3_indicators <- lapply(objective_3_indicators, as.numeric)

# Create a new data frame with the numeric columns
objective_3_numeric <- as.data.frame(objective_3_indicators)

# Check the structure of the new data frame
str(objective_2_numeric)



objective_3_indicators <- cleaned_data %>%
  select("Dinterest", "IR", "turnoveR", "MarketC", "Stockstraded", "Inflation", "turnoveR")
objective_3_indicators <- as.numeric(objective_3_indicators)

# Calculate correlation matrix for Objective 1 indicators
cor_matrix_obj_3 <- cor(objective_3_numeric, use = "complete.obs")

# Print correlation matrix
print("Correlation Matrix - Objective 1")
print(cor_matrix_obj_2)

# Visualize correlation matrix
corrplot(cor_matrix_obj_3, method = "color", type = "upper", tl.col = "black", tl.srt = 45)






############  T-TEST OBJECTIVE 1 ##############


# Objective 1: Temporal Analysis of Economic Indicators
objective_1_indicators <- cleaned_data %>%
  select("MarketC")

# Check for missing values and convert to numeric
objective_1_indicators$MarketC <- as.numeric(objective_1_indicators$MarketC)

# Remove missing values
objective_1_indicators <- na.omit(objective_1_indicators)

# Check the trend in market capitalization over time
summary_stats_market_cap <- summary(objective_1_indicators)
print(summary_stats_market_cap)

# Perform a t-test to assess if the mean market capitalization is significantly different from zero
t_test_result_market_cap <- t.test(objective_1_indicators$MarketC, mu = 0)

# Print results
print("Summary Statistics - Market Capitalization")
print(summary_stats_market_cap)

print("T-test Result - Market Capitalization")
print(t_test_result_market_cap)


############  T-TEST OBJECTIVE 2 ##############
