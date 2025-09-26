# Cài đặt thư viện nếu chưa có
#install.packages(c("readr", "ggplot2", "dplyr"))  # Để đọc CSV, vẽ biểu đồ, và xử lý dữ liệu
library(readr)
library(ggplot2)
library(dplyr)

# Đọc dữ liệu từ file CSV (giả sử bạn copy dữ liệu vào file data1.csv)
data <- read_csv("data/data1.csv")

# Đổi tên cột cho dễ dùng (xóa dấu ngoặc và khoảng trắng)
colnames(data) <- c("Gioi_tinh", "Tuoi", "Thu_nhap", "BMI", "Dia_diem",
                    "Tieu_duong", "Tim_mach", "Gay_xuong_hong", "Thoi_gian_nam_vien")

# Kiểm tra dữ liệu
head(data)
dim(data)  # Nên ra 342 hàng, 9 cột

# Biến định lượng
quant_vars <- c("Tuoi", "BMI", "Thoi_gian_nam_vien")
summary(data[, quant_vars])  # Thống kê cơ bản

# Biến định tính (tần suất phần trăm)
qual_vars <- c("Gioi_tinh", "Thu_nhap", "Dia_diem", "Tieu_duong", "Tim_mach", "Gay_xuong_hong")
for (var in qual_vars) {
  print(paste("Tần suất cho", var))
  print(prop.table(table(data[[var]])) * 100)
}

# Trực quan hóa định lượng (histogram)
for (var in quant_vars) {
  ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(bins = 20, fill = "blue", color = "black") +
    labs(title = paste("Histogram của", var), x = var, y = "Tần suất") +
    theme_minimal()
  ggsave(paste(var, "_hist.png"))  # Lưu biểu đồ
}

# Trực quan hóa định tính (barplot)
for (var in qual_vars) {
  ggplot(data, aes(x = .data[[var]])) +
    geom_bar(fill = "green", color = "black") +
    labs(title = paste("Barplot của", var), x = var, y = "Số lượng") +
    theme_minimal()
  ggsave(paste(var, "_bar.png"))  # Lưu biểu đồ
}

# Mô hình hồi quy tuyến tính
lin_model <- lm(Thoi_gian_nam_vien ~ BMI, data = data)
summary(lin_model)

# Trực quan hóa
ggplot(data, aes(x = BMI, y = Thoi_gian_nam_vien)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Hồi quy: Thời gian nằm viện ~ BMI", x = "BMI", y = "Thời gian nằm viện") +
  theme_minimal()
ggsave("lin_reg_plot.png")

# Chuyển biến nhị phân
data$Gay_xuong_hong_bin <- ifelse(data$Gay_xuong_hong == "Có", 1, 0)
data$Tieu_duong_bin <- ifelse(data$Tieu_duong == "Có", 1, 0)
data$Gioi_tinh_bin <- ifelse(data$Gioi_tinh == "Nam", 1, 0)

# Mô hình 1: Gãy xương hông ~ Tiểu đường
log_model1 <- glm(Gay_xuong_hong_bin ~ Tieu_duong_bin, data = data, family = binomial)
summary(log_model1)
exp(coef(log_model1))  # Odds Ratios

# Mô hình 2: Tiểu đường ~ Giới tính
log_model2 <- glm(Tieu_duong_bin ~ Gioi_tinh_bin, data = data, family = binomial)
summary(log_model2)
exp(coef(log_model2))  # Odds Ratios