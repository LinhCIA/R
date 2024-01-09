"""
  Để biết được trung bình mức giá bán cao nhất của hãng giày Adidas tính bằng USD được đăng bán trên nền tảng StockX, chúng tôi đã chọn ngẫu nhiên 2000 mẫu và thấy trong đó có 537 người bán ở các mức giá như sau:
      Mức giá bán (x)	20-175	176-350	351-525	526-700
      Số lượng bán (f)	79	196	205	57
	Với độ tin cậy 99%, hãy ước lượng mức giá trung bình của sản phẩm này? Biết rằng đây là biến ngẫu nhiên có phân phối chuẩn.
"""

# Giải bài toán trên
# Tạo DataFrame mới với dữ liệu của hãng adidas
adidas <- data[data$brand == "adidas", ]
adidas

# Tạo các khoảng giá
price_range <- c(20, 175, 350, 525, 700)

# Sử dụng hàm cut để tạo cột "price_range"
adidas$price_range <- cut(adidas$max_all_trade_range, breaks = price_range, labels = c("20-175", "176-350", "351-525", "526-700"))

# Sử dụng hàm table để tạo bảng tần suất
count <- table(adidas$brand, adidas$price_range)
count

# Chuẩn bị tập dữ liệu
prices <- c(97.5, 262.5, 438.5, 613.5)
frequencies <- c(79, 196, 205, 57)


# Tính khoảng tin cậy 99%
# Tạo data frame từ dữ liệu
data <- data.frame(x = rep(prices, frequencies))

# Thực hiện t-test và ước lượng khoảng tin cậy
result <- t.test(data$x, conf.level = 0.99)
result

# In kết quả tính toán
cat("Khoảng tin cậy 99% cho giá trung bình là: [", result$conf.int[1], ", ", result$conf.int[2], "]\n")
