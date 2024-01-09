# Bài toán kiểm định trên R tương đối đơn giản khi ta chỉ cần chọn ngẫu nhiên 500 hàng trong tổng người bán giày Adidas, từ đó ta sử dụng hàm có sẵn trong R là t.test() để thự hiện kiểm định 2 mẫu. 

# Giải bài toán kiểm định
library(dbplyr)

data <- read.csv("D:/data/500_data.csv")

t <- t.test(data$avg_sale_price, mu=307.19)
t
