library(readxl)
library(tidyverse)
library(GGally)

data <- read_excel("../Data/ResearchData.xlsx")

# Scaling the monetary variables by dividing by 1 million

data_scaled <- data
data_scaled[, c("Debt service on external debt, total (TDS, current US$)", 
                "GDP (current US$)")] <- 
  data[, c("Debt service on external debt, total (TDS, current US$)", 
           "GDP (current US$)")] / 1e6

# Log transformation

data_transformed <- data_scaled
data_transformed[, c("Debt service on external debt, total (TDS, current US$)", 
                     "GDP (current US$)", 
                     "Real effective exchange rate index (2010 = 100)")] <- 
  log(data_scaled[, c("Debt service on external debt, total (TDS, current US$)", 
                      "GDP (current US$)", 
                      "Real effective exchange rate index (2010 = 100)")])

data_transformed <- as.data.frame(data_transformed)
colnames(data_transformed) <- c('Year','Debt', 'GDP', 'ExchangeRate')

head(data_transformed)

# Assign variables

y <- data_transformed$ExchangeRate
X <- as.matrix(data_transformed[, c("Debt", "GDP")])

# Summary of the data

summary(data_transformed)

# Create a subset for plotting

data_subset <- data_transformed[, c("ExchangeRate", "Debt", "GDP")]

# Create pair Scatter plot using GGally

ggpairs(data_subset, 
        title = "Scatter Plots of Exchange Rate and Predictors",
        progress = FALSE)


