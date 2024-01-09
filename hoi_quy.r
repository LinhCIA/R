```{r}
# Steps to set up and train a Multivariate Linear Regression Model


# # Step 1: Import package
library(dplyr)
library(tidyverse)
library(ggplot2)

# Step 2: Load Data 
data <- read.csv("C:/R/StockX/DATA/stockx.csv")

print(head(data, 10))

# Step 3: Select target characteristics and variables
features <- c('release_month', 'release_year', 'retail_price', 'volatility', 'number_of_sales', 'price_premium', 'avg_sale_price')
target <- 'last_sale'

# Step 4: Split data into training set and test set
model <- lm(paste(target, "~ ."), data = train_data)

# Step 5: Predict on the test set
predictions <- predict(model, newdata = test_data)

# Step 6: Calculate model evaluation indexes
eval_metrics <- function(y_true, y_pred) {
  mae <- mean(abs(y_pred - y_true))
  rmse <- sqrt(mean((y_pred - y_true)^2))
  r2 <- cor(y_pred, y_true)^2
  
  return(c(MAE = mae, RMSE = rmse, R2 = r2))
}

metrics <- eval_metrics(test_data[[target]], predictions)
cat("Mean Absolute Error:", metrics["MAE"], "\n")
cat("Root Mean Squared Error:", metrics["RMSE"], "\n")
cat("R-squared:", metrics["R2"], "\n")

# Step 7: Draw a linear chart of the model
plot_data <- tibble(Actual = test_data[[target]], Predicted = predictions)
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = 'green', size = 3, alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue", size = 1) +
  labs(x = 'Giá trị thực tế', y = 'Giá trị dự đoán', 
       title = 'Biểu đồ QQ-plot cho mô hình hồi quy tuyến tính') +
  theme_minimal()

print(plot_data)

```
