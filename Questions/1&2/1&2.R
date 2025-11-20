
library(tidyverse)
library(corrplot)

df<- read.csv("realistic_bmw_data.csv")

print(head(df))
missing_values <- colSums(is.na(df))
print(missing_values)

num_duplicates <- sum(duplicated(df))
print(num_duplicates)

# CORRELATION MATRIX AND HEATMAP
numeric_df <- df %>% select_if(is.numeric)

correlation_matrix <- cor(numeric_df)


corrplot(correlation_matrix, 
         method = "color",         
         type = "upper",           # upper half of the matrix
         addCoef.col = "black",    #  correlation num on top of squares
         tl.col = "black",         # 
         tl.srt = 45,              # Rotate txt labels 45 deg
         title = "Correlation Matrix of BMW Sales Data",
         mar = c(0, 0, 1, 0))       # title margins


# VISUALIZATION FOR QUESTION 1: PRICE DETERMINANTS

#  1a. Scatter Plot: Mileage vs. Price 

ggplot(df, aes(x = Mileage_KM, y = Price_USD)) +
  geom_point(alpha = 0.2) +                  #  transparent points
  geom_smooth(method = "lm", color = "red") + #  linear trend line
  labs(title = " Mileage vs Price") +
  theme_minimal()

# 1b. Boxplot: Transmission vs. Price 

ggplot(df, aes(x = Transmission, y = Price_USD, fill = Transmission)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Transmission Type") +
  theme_minimal() +
  theme(legend.position = "none")



# VISUALIZATION FOR QUESTION 2: SALES VOLUME DETERMINANTS

# Scatter Plot: Price vs. Sales Volume
ggplot(df, aes(x = Price_USD, y = Sales_Volume)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "  Price vs Sales Volume ") +
  theme_minimal()