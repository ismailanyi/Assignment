library(dplyr)
library(randomForest)

car_data <- read.csv("bmw_sales_data.csv")

str(car_data)
summary(car_data)

car_data <- car_data %>%
  mutate(
    Transmission = as.factor(Transmission),
    Fuel_Type = as.factor(Fuel_Type),
    Region = as.factor(Region),
    Model = as.factor(Model),
    Sales_Classification = as.factor(Sales_Classification)
  )

predictors <- car_data %>%
  select(Year, Mileage_KM, Engine_Size_L, Price_USD, Transmission, Fuel_Type, Region, Model, Sales_Classification)

predictors <- na.omit(predictors)

str(predictors)

set.seed(123)

sales_model <- randomForest(
  sales_classification ~ .,
  data = predictors,
  ntree = 500,       
  importance = TRUE 
)

print(sales_model)

importance_scores <- importance(sales_model)
print(importance_scores)

varImpPlot(sales_model, main = "Top Predictors for Sales Classification")