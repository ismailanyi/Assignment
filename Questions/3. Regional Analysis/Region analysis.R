library('tidyverse')
library(ggcorrplot)

bmw <- read.csv("D:/Downloads/bmw_sales_updated.csv")
View(bmw)

str(bmw)
summary(bmw)

region_summary <- bmw %>%
  group_by(Region) %>% 
  summarise(
    Avg_Price = mean(Price_USD, na.rm = TRUE), 
    Avg_Sales_Volume = mean(Sales_Volume, na.rm = TRUE)
  ) %>% 
  arrange(desc(Avg_Price))

print(region_summary)

ggplot(region_summary, aes(x = reorder(Region, -Avg_Price), 
                           y = Avg_Price, 
                           color = Region)) + 
  geom_point(size = 4) + 
  geom_text(aes(label = round(Avg_Price, 1)), hjust = -0.3, size = 4) +
  labs(title = 'Average Used BMW Price by Region', 
       x = 'Region', y = 'Average Price (USD)') + 
  theme_minimal() + 
  theme(legend.position = 'none')

ggplot(region_summary, aes(x = reorder(Region, -Avg_Sales_Volume), 
                           y = Avg_Sales_Volume, 
                           color = Region)) + 
  geom_point(size = 4) + 
  geom_text(aes(label = round(Avg_Sales_Volume, 1)), hjust = -0.3, size = 4) +
  labs(title = 'Average Sales Volume by Region', 
       x = 'Region', y = 'Average Sales Volume') + 
  theme_minimal() + 
  theme(legend.position = 'none')

correlation <- cor(region_summary$Avg_Price, region_summary$Avg_Sales_Volume, method = 'pearson')
print(correlation)

ggplot(region_summary, aes(x = Avg_Price, 
                           y = Avg_Sales_Volume, 
                           color = Region)) + 
  geom_point(size = 4) + 
  geom_smooth(method = 'lm', se = FALSE, color = 'black', linetype = 'dashed') + 
  labs(
    title = 'Correlation Between Average BMW Price and Sales Volume by Region', 
    x = 'Average Price (USD)', 
    Y = 'Average Sales Volume'
  ) + 
  theme_minimal()

trend_data <- bmw %>%
  group_by(Year, Region) %>%
  summarise(Avg_Price = mean(Price_USD, na.rm = TRUE),
            Avg_Sales_Volume = mean(Sales_Volume, na.rm = TRUE)) %>%
  ungroup()

print(trend_data)

ggplot(trend_data, aes(x = Year, y = Avg_Price, color = Region, group = Region)) +
  geom_line(size = 1.1) +
  geom_point() +
  labs(title = "Trends in Used BMW Prices Across Regions (2010–2024)",
       x = "Year", y = "Average Price (USD)") +
  theme_minimal()

ggplot(trend_data, aes(x = Year, y = Avg_Sales_Volume, color = Region, group = Region)) +
  geom_line(size = 1.1) +
  geom_point() +
  labs(title = "Trends in BMW Sales Volume Across Regions (2010–2024)",
       x = "Year", y = "Average Sales Volume") +
  theme_minimal()