```{r}
install.packages("e1071")
install.packages("plotly")
install.packages("htmltools")
```


```{r}
library(e1071)
library(plotly)
library(dbplyr)
library(ggplot2)
library(tidyverse)
library(caTools)
```

```{r}
data <- read.csv("D:/data/stockx_fixed.csv", header=TRUE)
```

```{r}
features <- data[c('last_sale', 'min_12_month_trade_rage', 'max_12_month_trade_rage','avg_sale_price','price_premium')]
target <- data['retail_price']
```

```{r}
pca <- prcomp(features, scale. = TRUE)
pca <- pca$x[,1:2]
pca <- data.frame(pca)
pca$target <- target$retail_price
```

```{r}
set.seed(111)
split <- sample.split(pca$target, SplitRatio = 0.8)
train_data <- subset(pca, split == TRUE)
test_data <- subset(pca, split == FALSE)
```

```{r}
model <- svm(train_data$target~., data=train_data, kernel='radial')
pred_data <- predict(model, newdata=test_data)
summary(model)
```

```{r}
mse <- mean((pred_data - test_data$target)^2)
mse
```

```{r}
r_squared <- 1 - sum((pred_data - test_data$target)^2) / sum((mean(test_data$target) - test_data$target)^2)
r_squared
```

```{r}
# Create a 3D scatter plot
plot <- plot_ly(train_data, x = ~PC1, y = ~PC2, z = ~target, color = ~target, type = "scatter3d", mode = "markers")

# Customize the layout
layout <- layout(scene = list(aspectmode = "cube"))

# Display the plot
fig <- layout(plot, layout)
fig
```
