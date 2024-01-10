"""
	Bài toán ước lượng tỷ lệ: Để ước lượng số người bán giày mang thương hiệu Nike từ trước đến nay, 
	chúng tôi đã lấy ra ngẫu nhiên mẫu 2000 người bán và lọc ra trong đó có 464 người bán giày Nike. 
	Với độ tin cậy 95% hãy ước lượng tỉ lệ bán giày Nike trên nền tảng StockX.
"""


# Giải quyết bài toán
#Tải thư viện "survey"
install.packages("binom")
library("dplyr")
library(binom)
setwd("D:/data/")

# Load dữ liệu
data <- read.csv(file="stockx.csv", header=TRUE)
set.seed(11)

# Hàm lấy mẫu
lay_mau <- function(data) {
  indices <- sample(1:nrow(data), 2000, replace = FALSE)
  mau <- data[indices, ]
  write_csv(mau, "D:/data/2000data_mau.csv", col_names = TRUE, append = FALSE)
}

# Gọi hàm lấy mẫu
lay_mau(data)


# Đọc mẫu thu thập được
data <- read.csv(file="2000data.csv", header=TRUE)

# Các dữ liệu cần thiết
# Số người bán Nike
so_nguoi_ban_nike <- nrow(data %>% filter(brand=="Nike"))
so_nguoi_ban_nike

#Số mẫu
so_mau <- nrow(data)
so_mau

# Tiến hành ước lượng tỉ lệ
khoang_tin_cay <- binom.confint(so_nguoi_ban_nike, so_mau, method = "wilson")
khoang_tin_cay
