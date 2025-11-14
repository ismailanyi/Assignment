library(tidyverse)
library(corrplot)

# 2. Load the dataset

file_path <- "realistic_bmw_data.csv"
df1 <- read.csv(file_path)

library(corrplot)
numeric_cols_realistic <- df %>% select_if(is.numeric)
correlation_matrix_realistic <- cor(numeric_cols_realistic)
corrplot(correlation_matrix_realistic, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Correlation Heatmap (Realistic Data)", mar=c(0,0,1,0))
# Be sure you have already created the df_realistic dataframe from the previous step!
