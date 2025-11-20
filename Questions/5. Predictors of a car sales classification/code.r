install.packages("dplyr")
install.packages("randomForest")

library(dplyr)
library(randomForest)

car_data <- read.csv("bmw_sales_data.csv")

str(car_data)
summary(car_data)

car_data <- car_data %>%
  mutate(
    transmission = as.factor(transmission),
    fuel_type = as.factor(fuel_type),
    region = as.factor(region),
    model = as.factor(model),
    sales_classification = as.factor(sales_classification)
  )

predictors <- car_data %>%
  select(year, mileage_km, engine_size_l, price_usd, transmission, fuel_type, region, model, sales_classification)

predictors <- na.omit(predictors)

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