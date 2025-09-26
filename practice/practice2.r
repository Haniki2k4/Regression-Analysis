# Bài tập thực hành số 2: Mối quan hệ giữa các yếu tố lối sống, sinh hoạt và chỉ số sức khỏe với huyết áp tâm thu

# Đọc dữ liệu
data <- read.csv("data/data2.csv", header = TRUE)
print(colnames(data))

#Xử lý biến
data$UongRuou <- factor(data$UongRuou, levels = c("Khong", "It", "Trung Binh", "Nhieu"), labels = c(1, 2, 3, 4))
data$UongRuou <- as.numeric(as.character(data$UongRuou))
data$HutThuoc <- as.numeric(data$HutThuoc)
data$TienSuBenhTim <- as.numeric(data$TienSuBenhTim)
data$Diabet <- as.numeric(data$Diabet)

data <- na.omit(data)

#------------------------------------------------------------------------
#  1. Mô hình hồi quy tuyến tính dự đoán HuyetApTamThu dựa trên biến Tuoi
#------------------------------------------------------------------------
cat("\nMô hình hồi quy tuyến tính dự đoán HuyetApTamThu dựa trên biến Tuoi\n")
model_1 <- lm(HuyetApTamThu ~ Tuoi, data = data)
summary(model_1)
# coef_tuoi <- coef(model_1)["Tuoi"]
# cat("Hệ số hồi quy của Tuoi:", coef_tuoi, "\n")

# Nhận xét:
# Hệ số hồi quy của Tuoi = 0.47652, dựa vào p-value = 2.2e-16< 0.05 có thể kết luận có ý nghĩa thống kê.
# Diên giải: Mỗi tăng 1 tuổi, huyết áp tâm thu tăng trung bình 0.47652 mmHg, giữ các yếu tố khác không đổi.
# R-squared = 0.2494 cho thấy mô hình giải thích được 24.94% biến thiên của HuyetApTamThu.


#------------------------------------------------------------------------
#  2. Mô hình hồi quy tuyến tính dự đoán HuyetApTamThu dựa trên tất cả các biến độc lập
#------------------------------------------------------------------------
cat("\nMô hình hồi quy tuyến tính dự đoán HuyetApTamThu dựa trên tất cả các biến độc lập\n")
model_2 <- lm(HuyetApTamThu ~ Tuoi + BMI + GioTapTheDuc + SoBuaAnCheBien + HutThuoc + UongRuou + GioNgu + MucDoStress + TienSuBenhTim + Diabet, data = data)
summary(model_2)

# Nhận xét:
# - Ngoại trừ biến GioNgu thì các biến đều có ý nghĩa thống kê với mức p-value < 0.001 (ký hiệu ***)
#   Trong các biến, biến Tuoi có t-value lớn nhất (t = 18.985), cho thấy đây là biến có ý nghĩa thống kê mạnh nhất trong mô hình.

# - Mô hình đơn biến: R-sqared = 0.2494
#   Mô hình đa biến: R-squared = 0.701
#   Như vậy, mô hình đa biến giải thích được nhiều phương sai của HuyetApTamThu hơn so với mô hình đơn biến.

# - Giải thích ý nghĩa hệ số hồi quy cho BMI và HutThuoc
#  BMI: Hệ số = 1.16437, p-value < 2e-16
#  Khi BMI tăng 1 đơn vị, huyết áp tâm thu trung bình tăng 1.16 mmHg (giữ các yếu tố khác không đổi).

#  HutThuoc: Hệ số = 5.34467, p-value ≈ 9.57e-11
#  Người hút thuốc có huyết áp tâm thu trung bình cao hơn 5.34 mmHg so với người không hút thuốc (giữ các yếu tố khác không đổi).


#------------------------------------------------------------------------
#  3.  Kiểm tra giả định của mô hình hồi quy đa biến
#------------------------------------------------------------------------
library(car)
library(lmtest)
cat("\nKiểm tra giả định của mô hình hồi quy đa biến\n")
# Tính tuyến tính: Biểu đồ phân tán + phần dư
vars <- c("Tuoi", "BMI", "GioTapTheDuc", "SoBuaAnCheBien", "HutThuoc", "UongRuou", "GioNgu", "MucDoStress", "TienSuBenhTim")
for (v in vars) {
  plot(data[[v]], data$HuyetApTamThu, main = paste("Scatter:", v, "~ HuyetApTamThu"), xlab = v, ylab = "HuyetApTamThu")
  abline(lm(HuyetApTamThu ~ data[[v]], data = data), col = "red")
}
plot(model_2$fitted.values, model_2$residuals, main = "Residuals vs Fitted")
abline(h = 0, col = "red"
)

# Phân phối phần dư gần chuẩn: QQ plot và Shapiro-Wilk test
qqnorm(model_2$residuals)
qqline(model_2$residuals)
shapiro.test(model_2$residuals)

# c. Phương sai phần dư đồng nhất: Breusch-Pagan test (cần library(lmtest))
bptest(model_2)

# d. Đa cộng tuyến: VIF (cần library(car))
car::vif(model_2)

#------------------------------------------------------------------------
#  4. Trình bày kết quả của mô hình đa biến cuối cùng (sau khi xử lý đa cộng tuyến)
#------------------------------------------------------------------------
cat("\nMô hình hồi quy tuyến tính dự đoán HuyetApTamThu sau khi xử lý đa cộng tuyến\n")
model_final <- lm(HuyetApTamThu ~ Tuoi + GioTapTheDuc + SoBuaAnCheBien + HutThuoc + UongRuou + GioNgu + MucDoStress + TienSuBenhTim + Diabet, data = data)
summary(model_final)
car::vif(model_final)  # Kiểm tra VIF mới