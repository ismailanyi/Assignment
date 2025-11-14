# Install the packages if you haven't already
install.packages("dplyr")
install.packages("randomForest")

# Load the packages into your R session
library(dplyr)
library(randomForest)


# --------------------------------------------------------------------------------
# --- Data Loading and Inspection ---
# --------------------------------------------------------------------------------
# Load your dataset (make sure the CSV file is in your working directory)
# Let's assume your file is named 'bmw_sales_data.csv'
car_data <- read.csv("bmw_sales_data.csv")

# It's good practice to inspect the data to see its structure
str(car_data)
summary(car_data)

# --- Data Preparation ---
# The model needs to know that categorical variables are categories (factors in R).
# The target variable 'sales_classification' must also be a factor.
car_data <- car_data %>%
  mutate(
    Transmission = as.factor(Transmission),
    Fuel_Type = as.factor(Fuel_Type),
    Region = as.factor(Region),
    Model = as.factor(Model),
    Sales_Classification = as.factor(Sales_Classification)
  )

# Let's select the relevant columns for prediction. We will exclude sales_volume
predictors <- car_data %>%
  select(Year, Mileage_KM, Engine_Size_L, Price_USD, Transmission, Fuel_Type, Region, Model, Sales_Classification)

# Check for and remove any rows with missing values
predictors <- na.omit(predictors)

# Check the structure of your final "predictors" data frame to be sure
str(predictors)


# --------------------------------------------------------------------------------
# --- Model Building ---
# --------------------------------------------------------------------------------
# Set a seed for reproducibility, so you get the same result every time you run it
set.seed(123)

# Build the Random Forest model
# We are predicting 'sales_classification' using all other variables in the 'predictors' dataframe.
# The `.` means "use all other columns as predictors".
# `importance=TRUE` is CRITICAL. It tells the model to calculate which predictors are the best.
sales_model <- randomForest(
  sales_classification ~ .,
  data = predictors,
  ntree = 500,       # Number of "trees" to build. 500 is a good starting point.
  importance = TRUE  # This is essential for our question!
)

# You can print the model to see a summary of its performance
print(sales_model)


# --------------------------------------------------------------------------------
# --- Identifying Best Predictors ---
# --------------------------------------------------------------------------------
# The model has calculated the importance of each variable.
# Let's see the raw scores.
importance_scores <- importance(sales_model)
print(importance_scores)

# Now, let's create a plot of the variable importance.
# This plot is the KEY result for your question.
varImpPlot(sales_model, main = "Top Predictors for Sales Classification")