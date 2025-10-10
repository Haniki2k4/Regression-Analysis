library(haven)      # đọc file .sav
library(dplyr)      # xử lý dữ liệu
library(ggplot2)    # trực quan hóa
library(janitor)    # tabyl tần số
library(tidyr)      # reshape dữ liệu
library(scales)     # hiển thị %
library(knitr)
library(kableExtra)
library(MASS)
library(VIM)
library(zoo)      # Cần cho việc nội suy na.approx()
library(forecast) # Thư viện chính cho mô hình chuỗi thời gian (ARIMA, Holt)
library(ggfortify)# Để vẽ đồ thị forecast đẹp hơn với ggplot2
library(purrr)


# -------------------------------
#   1. Kết quả làm sạch dữ liệu
# -------------------------------

read_hh <- function(path, vars, mics) {
  df <- read_sav(path)
  
  filter_condition <- switch(
    as.character(mics),
    "6" = quote(HH12 == 1 & HH46 == 01),
    "5" = quote(HH9 == 01),
    "4" = quote(HH9 == 01),
    "3" = quote(hh9 == 1),
    "2" = quote(HI10 == 1),
    NULL
  )
  
  # Nếu có điều kiện lọc, áp dụng; nếu không, giữ nguyên
  if (!is.null(filter_condition)) {
    df <- df %>% filter(!!filter_condition)
  }
  
  # Chỉ giữ các biến cần thiết
  df <- df %>% dplyr::select(any_of(vars))
  
  return(df)
}

read_ch <- function(path, vars, mics) {
  df <- read_sav(path) 
  
  # Xác định điều kiện lọc theo từng bộ MICS
  filter_condition <- switch(
    as.character(mics),
    "6" = quote(UF10 == 1),
    "5" = quote(UF9 == 1),
    "4" = quote(UF9 == 1),
    NULL
  )
  
  # Áp dụng lọc nếu có
  if (!is.null(filter_condition)) {
    df <- df %>% filter(!!filter_condition)
  }
  
  # Chọn các biến cần thiết
  df <- df %>% dplyr::select(any_of(vars))
  
  return(df)
}

# --- Đọc dữ liệu các năm MICS ---
# MICS6
vars_hh_ms6 <- c("HH1","HH2","HH6","WS1","WS11","helevel","HC2","windex5","windex5r","windex5u","HH5Y")
hh6 <- read_hh("E:/regression-analysis/final-exam/Viet Nam MICS6 Datasets/Viet Nam MICS6 SPSS Datasets/hh.sav",
               vars_hh_ms6, mics = 6)
ch6 <- read_ch(
  "E:/regression-analysis/final-exam/Viet Nam MICS6 Datasets/Viet Nam MICS6 SPSS Datasets/ch.sav",
  vars = c("HH1", "HH2", "CA1", "UF10"),
  mics = 6
)

# MICS5
vars_hh_ms5 <- c("HH1","HH2","HH6","WS1","WS8","helevel","windex5","HH5Y")
hh5 <- read_hh("E:/regression-analysis/final-exam/Regression-Analysis/final-exam/Viet Nam_MICS5_Datasets/Viet Nam MICS 2013-14 SPSS Datasets/hh.sav",
               vars_hh_ms5, mics = 5)
ch5 <- read_ch(
  "E:/regression-analysis/final-exam/Regression-Analysis/final-exam/Viet Nam_MICS5_Datasets/Viet Nam MICS 2013-14 SPSS Datasets/ch.sav",
  vars = c("HH1", "HH2", "CA1", "UF9"),
  mics = 5
)

# MICS4
vars_hh_ms4 <- c("HH1","HH2","HH6","WS1","WS8","helevel","windex5","HH5Y")
hh4 <- read_hh("E:/regression-analysis/final-exam/Regression-Analysis/final-exam/Vietnam_MICS4_Datasets/Vietnam MICS 2010-2011 SPSS Datasets/hh.sav",
               vars_hh_ms4, mics = 4)
ch4 <- read_ch(
  "E:/regression-analysis/final-exam/Regression-Analysis/final-exam/Vietnam_MICS4_Datasets/Vietnam MICS 2010-2011 SPSS Datasets/ch.sav",
  vars = c("HH1", "HH2", "CA1", "UF9"),
  mics = 4
)

# MICS3
vars_hh_ms3 <- c("diaban","hh2","hh6","ws1","ws7","helevel","wlthind5","hh5y")
hh3 <- read_hh("E:/regression-analysis/final-exam/Regression-Analysis/final-exam/Vietnam_Datasets/Vietnam MICS 2006 SPSS Datasets/hh.sav",
               vars_hh_ms3, mics = 3)
ch3 <- read_ch(
  "E:/regression-analysis/final-exam/Regression-Analysis/final-exam/Vietnam_Datasets/Vietnam MICS 2006 SPSS Datasets/ch.sav",
  vars = c("diaban", "hh2", "ca1"),
  mics = 3
)

# MICS2
vars_hh_ms2 <- c("HI1","HI2","HI6","WS1","WS3","ED16A","WLTHIND5")
hh2 <- read_hh("E:/regression-analysis/final-exam/Regression-Analysis/final-exam/Viet Nam 2000 MICS_Datasets/hhVI.sav",
               vars_hh_ms2, mics = 2)
ch2 <- read_ch(
  "E:/regression-analysis/final-exam/Regression-Analysis/final-exam/Viet Nam 2000 MICS_Datasets/chVI.sav",
  vars = c("HI1", "HI2", "CI1"),
  mics = 2
)

# Kiểm tra missing
# Tính tỷ lệ missing
hh_missing <- hh6 %>% summarise(across(everything(), ~ mean(is.na(.))*100))
ch_missing <- ch6 %>% summarise(across(everything(), ~ mean(is.na(.))*100))

kable(hh_missing, caption="Bảng 1. Tỷ lệ thiếu dữ liệu trong hh.sav") %>% kable_styling()
kable(ch_missing, caption="Bảng 2. Tỷ lệ thiếu dữ liệu trong ch.sav") %>% kable_styling()

# --- Tạo biến tiêu chảy ở trẻ ---
ch6_clean <- ch6 %>% 
  mutate(diarrhea = if_else(CA1 == 1, 1, 0))

# Gom theo hộ: hộ có ít nhất 1 trẻ bị tiêu chảy
diarrhea_hh <- ch6_clean %>% 
  group_by(HH1, HH2) %>% 
  summarise(
    any_diarrhea = if (all(is.na(diarrhea))) NA_real_ else max(diarrhea, na.rm = TRUE),
    .groups = "drop"
  )

# Ghép vào dữ liệu hộ
hh6_clean <- hh6
hh6_clean <- hh6_clean %>% 
  left_join(diarrhea_hh, by = c("HH1", "HH2"))

# --- Tạo biến nước sạch và vệ sinh ---
hh6_clean <- hh6_clean %>% 
  mutate(
    safe_water = case_when(
      WS1 %in% c(11,12,13,14,21,31,41,51,72,91,92) ~ 1,
      WS1 %in% c(32,42,61,71,81,96,99) ~ 0,
      TRUE ~ NA_real_
    ),
    improved_sanitation = case_when(
      WS11 %in% c(11,12,21,22,31) ~ 1,
      WS11 %in% c(13,14,18,23,41,51,95,96,99) ~ 0,
      TRUE ~ NA_real_
    )
  )


# -------------------------------
#   2. Thống kê mô tả và trực quan hóa
# -------------------------------

# Tần số nước sạch & nhà tiêu
tabyl(hh6_clean, safe_water) %>%
  mutate(safe_water = case_when(
    safe_water == 1 ~ "Có",
    safe_water == 0 ~ "Không",
    TRUE ~ as.character(safe_water)
  )) %>%
  kable(caption = "Bảng 2. Tần số hộ có nước sạch",
        align = "lcc")

# Bảng Improved Sanitation
tabyl(hh6_clean, improved_sanitation) %>%
  mutate(improved_sanitation = case_when(
    improved_sanitation == 1 ~ "Có",
    improved_sanitation == 0 ~ "Không",
    TRUE ~ as.character(improved_sanitation)
  )) %>%
  kable(caption = "Bảng 3. Tần số hộ có nhà tiêu hợp vệ sinh",
        align = "lcc") 

# Tỷ lệ hộ sử dụng nước sạch
safe_tab <- hh6_clean %>% 
  filter(!is.na(safe_water)) %>% 
  count(safe_water) %>% 
  mutate(pct = round(n / sum(n) * 100, 1),
         label = paste0(pct, "%"))

ggplot(safe_tab, aes(x = "", y = n, fill = factor(safe_water))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4) +
  scale_fill_manual(
    name = "Nước sạch",
    values = c("0" = "#e74c3c", "1" = "#2ecc71"),  # màu: đỏ/ xanh
    labels = c("0" = "Không", "1" = "Có")         # đổi nhãn
  ) +
  labs(title = "Tỉ lệ hộ sử dụng nguồn nước sạch tại Việt Nam năm 2021") +
  theme_void()

# Tỷ lệ hộ có nhà tiêu hợp vệ sinh
san_tab <- hh6_clean %>% 
  filter(!is.na(improved_sanitation)) %>% 
  count(improved_sanitation) %>% 
  mutate(pct = round(n / sum(n) * 100, 1),
         label = paste0(pct, "%"))

ggplot(san_tab, aes(x = "", y = n, fill = factor(improved_sanitation))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4) +
  scale_fill_manual(
    name = "Nhà tiêu hợp vệ sinh",
    values = c("0" = "#e67e22", "1" = "#3498db"),
    labels = c("0" = "Không", "1" = "Có")
  ) +
  labs(title = "Tỉ lệ hộ có Nhà tiêu hợp vệ sinh tại Việt Nam năm 2021") +
  theme_void()

# Biểu đồ cột so sánh theo khu vực
# Nước sạch theo khu vực
water_by_area <- hh6_clean %>% 
  filter(!is.na(safe_water)) %>% 
  group_by(HH6, safe_water) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  group_by(HH6) %>% 
  mutate(pct = n / sum(n) * 100)

ggplot(water_by_area, aes(x = factor(HH6), y = pct, fill = factor(safe_water))) +
  geom_col() +
  geom_text(aes(label = paste0(round(pct,1), "%")), 
            position = position_stack(vjust = 0.5),
            color = "white", size = 4) +
  scale_fill_manual(
    name = "Nước sạch",
    values = c("0" = "#e74c3c", "1" = "#2ecc71"),  # màu đỏ và xanh
    labels = c("0" = "Không", "1" = "Có")         # nhãn hiển thị
  ) +
  labs(
    title = "Tỷ lệ hộ có sử dụng Nguồn nước sạch theo khu vực tại Việt Nam năm 2021",
    x = "Khu vực (1 = Thành thị, 2 = Nông thôn)",
    y = "Tỷ lệ (%)"
  ) +
  theme_minimal()

# Nhà tiêu theo khu vực
san_by_area <- hh6_clean %>% 
  filter(!is.na(improved_sanitation)) %>% 
  group_by(HH6, improved_sanitation) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  group_by(HH6) %>% 
  mutate(pct = n / sum(n) * 100)

ggplot(san_by_area, aes(x = factor(HH6), y = pct, fill = factor(improved_sanitation))) +
  geom_col() +
  geom_text(aes(label = paste0(round(pct,1), "%")), 
            position = position_stack(vjust = 0.5),
            color = "white", size = 4) +
  scale_fill_manual(
    name = "Nhà tiêu hợp vệ sinh",
    values = c("0" = "#e67e22", "1" = "#3498db"),  # màu cam và xanh dương
    labels = c("0" = "Không", "1" = "Có")
  ) +
  labs(
    title = "Tỷ lệ hộ có Nhà tiêu hợp vệ sinh phân theo khu vực tại Việt Nam năm 2021",
    x = "Khu vực (1 = Thành thị, 2 = Nông thôn)",
    y = "Tỷ lệ (%)"
  ) +
  theme_minimal()


# -------------------------------
# 3. Kiểm định Chi-squared & Hồi quy logistic
# -------------------------------

# Kiểm định Chi-square
chisq_res <- chisq.test(hh6_clean$safe_water, hh6_clean$any_diarrhea)
knitr::kable(as.data.frame(chisq_res$observed),
             caption="Bảng 4. Bảng chéo giữa Nước sạch và Tiêu chảy")
chisq_res

# Hồi quy logistic
model1 <- glm(improved_sanitation ~ windex5 + helevel + HH6 + HC2,
              data = hh6_clean,
              family = binomial)

summary(model1)

model2 <- glm(improved_sanitation ~ windex5r + helevel + HC2,
              data = hh6_clean,
              family = binomial)

summary(model2)

model3 <- glm(improved_sanitation ~ windex5u + helevel + HC2,
              data = hh6_clean,
              family = binomial)

summary(model3) 

# -------------------------------
# 4. Sự thay đổi của các vấn đề trong 20 năm (2000-2021)
# -------------------------------

# Xử lý MICS 2000
clean_mics2 <- hh2 %>%
  dplyr::select(
    area = HI6,
    water_source = WS1,
    toilet_type = WS3,
    wealth_quintile = WLTHIND5 
  ) %>%
  mutate(
    year = 2000,
    improved_water = ifelse(water_source %in% c(01,02,03,04,05,06,08), 1, 0),
    improved_sanitation = ifelse(toilet_type %in% c(1,2,3,4), 1, 0)
  )

# Xử lý MICS 2006
clean_mics3 <- hh3 %>%
  dplyr::select(
    area = hh6,
    water_source = ws1,
    toilet_type = ws7,
    wealth_quintile = wlthind5, 
    education = helevel,
    year = hh5y
  ) %>%
  mutate(
    improved_water = ifelse(water_source %in% c(11,12,13,21,31,41,51), 1, 0),
    improved_sanitation = ifelse(toilet_type %in% c(11,12,13,21,22,31), 1, 0)
  )


# Xử lý MICS 2010-11
clean_mics4 <- hh4 %>%
  dplyr::select(
    area = HH6,
    water_source = WS1,
    toilet_type = WS8,
    wealth_quintile = windex5,
    education = helevel,
    year = HH5Y
  ) %>%
  mutate(
    improved_water = ifelse(water_source %in% c(11,12,13,14,21,31,41,51,91), 1, 0),
    improved_sanitation = ifelse(toilet_type %in% c(11,12,21,22,31), 1, 0)
  )

# Xử lý MICS 2013-14
clean_mics5 <- hh5 %>%
  dplyr::select(
    area = HH6,
    water_source = WS1,
    toilet_type = WS8,
    wealth_quintile = windex5,
    education = helevel,
    year = HH5Y
  ) %>%
  mutate(
    improved_water = ifelse(water_source %in% c(11,12,13,14,21,31,41,51), 1, 0),
    improved_sanitation = ifelse(toilet_type %in% c(11,12,21,22,31), 1, 0)
  )

# Xử lý MICS 2021
clean_mics6 <- hh6 %>%
  dplyr::select(
    area = HH6,
    water_source = WS1,
    toilet_type = WS11,
    wealth_quintile = windex5,
    education = helevel,
    year = HH5Y
  ) %>%
  mutate(
    improved_water = ifelse(water_source %in% c(11,12,13,14,21,31,41,51,91,92), 1, 0),
    improved_sanitation = ifelse(toilet_type %in% c(11,12,21,22,31), 1, 0)
  )

all_mics_data <- bind_rows(
  clean_mics6,
  clean_mics5,
  clean_mics4,
  clean_mics3,
  clean_mics2
)

# Tính toán tỷ lệ phần trăm
summary_stats <- all_mics_data %>%
  filter(!is.na(area)) %>%
  group_by(year, area) %>%
  summarise(
    pct_improved_water = mean(improved_water, na.rm = TRUE),
    pct_improved_sanitation = mean(improved_sanitation, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    year = as.numeric(year),
    pct_improved_water = as.numeric(pct_improved_water),
    area = area
  )
# Biểu đồ xu hướng nguồn nước
ggplot(summary_stats, aes(x = year, y = pct_improved_water, color = factor(area), group = factor(area))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0.7, 1),
    breaks = seq(0.5, 1, 0.1)
  ) +
  scale_color_manual(
    name = "Khu vực",                       # Tiêu đề chú thích
    values = c("1" = "#00bec5", "2" = "#f9776d"), # Màu: xanh lam / xanh lá
    labels = c("1" = "Thành thị", "2" = "Nông thôn")      # Gán nhãn mới
  ) +
  labs(
    title = "Mô hình xu hướng Tiếp cận Nguồn nước sạch (2000–2021)",
    subtitle = "Đường nét đứt thể hiện xu thế tuyến tính",
    x = "Năm",
    y = "Tỷ lệ Hộ gia đình",
    color = "Khu vực"
  ) +
  theme_minimal(base_size = 14)
# Biểu đồ xu hướng nhà tiêu
ggplot(summary_stats, aes(x = year, y = pct_improved_sanitation, color = factor(area), group = factor(area))) +
  geom_line(linewidth = 1.2) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent, limits = c(0.2, 1)) +
  scale_color_manual(
    name = "Khu vực",                       # Tiêu đề chú thích
    values = c("1" = "#00bec5", "2" = "#f9776d"), # Màu: xanh lam / xanh lá
    labels = c("1" = "Thành thị", "2" = "Nông thôn")      # Gán nhãn mới
  ) +
  labs(
    title = "Mô hình xu hướng Sử dụng Nhà tiêu Hợp vệ sinh (2000-2021)",
    subtitle = "Đường nét đứt thể hiện xu thế tuyến tính",
    x = "Năm", y = "Tỷ lệ Hộ gia đình", color = "Khu vực"
  ) + 
  theme_minimal(base_size = 14)

# Mô hình hóa và Dự báo bằng ARIMA
# Hàm tạo dự báo tự động
create_forecast <- function(data, value_col, area_filter, title_text) {
  ts_data_sparse <- data %>% filter(area == area_filter)
  full_years <- data.frame(year = min(data$year):max(data$year))
  
  ts_data_full <- full_years %>% left_join(ts_data_sparse, by = "year")
  ts_data_full$interpolated <- zoo::na.approx(ts_data_full[[value_col]], na.rm = FALSE)
  
  ts_object <- ts(ts_data_full$interpolated, start = min(data$year), frequency = 1)
  
  # Tạo nhãn khu vực
  area_label <- ifelse(area_filter == 1, "Thành thị", 
                       ifelse(area_filter == 2, "Nông thôn", as.character(area_filter)))
  
  # Mô hình và dự báo
  arima_model <- forecast::auto.arima(ts_object)
  arima_forecast <- forecast::forecast(arima_model, h = 10)
  
  # Biểu đồ dự báo
  plot <- forecast::autoplot(arima_forecast) +
    geom_point(data = ts_data_sparse, aes_string(x = "year", y = value_col),
               color = "red", size = 3) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = paste("Dự báo:", title_text, "-", area_label),
      subtitle = "Dữ liệu nội suy từ các điểm khảo sát (màu đỏ)",
      x = "Năm", y = "Tỷ lệ Hộ gia đình (%)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(family = "Times New Roman", face = "bold", size = 16),
      plot.subtitle = element_text(family = "Times New Roman", size = 13)
    )
  
  print(plot)
  cat(paste0("\n**Mô hình ARIMA cho ", title_text, " - ", area_label, "**\n"))
  print(summary(arima_model))
  
  return(arima_forecast)
}

f_water_rural <- create_forecast(summary_stats, "pct_improved_water", 2, "Tỷ lệ  sử dụng Nguồn nước sạch")
f_sanitation_rural <- create_forecast(summary_stats, "pct_improved_sanitation", 2, "Tỷ lệ tiếp cận Nhà tiêu Hợp vệ sinh")
f_water_urban <- create_forecast(summary_stats, "pct_improved_water", 1, "Tỷ lệ sử dụng Nguồn nước sạch")
f_sanitation_urban <- create_forecast(summary_stats, "pct_improved_sanitation", 1, "Tỷ lệ tiếp cận Nhà tiêu Hợp vệ sinh")

# Kiểm định Giả thuyết về Nước sạch và Bệnh Tiêu chảy qua 20 năm
# Tạo một hàm xử lý để tránh lặp code.
# Hàm này sẽ nhận vào dữ liệu hộ gia đình, dữ liệu trẻ em, năm, và một "bản đồ" tên biến.
process_mics_data <- function(hh_df, ch_df, year, vars_map) {
  # Đổi tên cột child data để nhất quán
  names(ch_df)[names(ch_df) == vars_map$id1_ch] <- "ID1"
  names(ch_df)[names(ch_df) == vars_map$id2_ch] <- "ID2"
  names(ch_df)[names(ch_df) == vars_map$diarrhea] <- "DIARRHEA_VAR"
  
  # Đổi tên cột household data để nhất quán
  names(hh_df)[names(hh_df) == vars_map$id1_hh] <- "ID1"
  names(hh_df)[names(hh_df) == vars_map$id2_hh] <- "ID2"
  names(hh_df)[names(hh_df) == vars_map$water] <- "WATER_VAR"
  
  # Tạo biến improved_water
  hh_df <- hh_df %>%
    mutate(
      improved_water = ifelse(WATER_VAR %in% vars_map$safe_water_codes, 1, 0)
    )
  
  # Tạo biến has_diarrhea từ child data
  ch_df <- ch_df %>%
    mutate(has_diarrhea = ifelse(DIARRHEA_VAR == 1, 1, 0))
  
  # Gộp hai bộ dữ liệu
  merged_df <- inner_join(
    hh_df %>% dplyr::select(ID1, ID2, improved_water),
    ch_df %>% dplyr::select(ID1, ID2, has_diarrhea),
    by = c("ID1", "ID2")
  ) %>%
    mutate(year = year)
  
  return(merged_df)
}

# Định nghĩa các biến và mã số khác nhau cho từng năm (giữ nguyên)
map_mics2 <- list(id1_hh="HI1", id2_hh="HI2", water="WS1", id1_ch="HI1", id2_ch="HI2", diarrhea="CI1", safe_water_codes = c(1,2,8,11,12,13))
map_mics3 <- list(id1_hh="diaban", id2_hh="hh2", water="ws1", id1_ch="diaban", id2_ch="hh2", diarrhea="ca1", safe_water_codes = c(11:14, 21, 31, 41, 51, 91))
map_mics4 <- list(id1_hh="HH1", id2_hh="HH2", water="WS1", id1_ch="HH1", id2_ch="HH2", diarrhea="CA1", safe_water_codes = c(11:14, 21, 31, 41, 51, 91))
map_mics5 <- list(id1_hh="HH1", id2_hh="HH2", water="WS1", id1_ch="HH1", id2_ch="HH2", diarrhea="CA1", safe_water_codes = c(11:14, 21, 31, 41, 51, 91))
map_mics6 <- list(id1_hh="HH1", id2_hh="HH2", water="WS1", id1_ch="HH1", id2_ch="HH2", diarrhea="CA1", safe_water_codes = c(11:14, 21, 31, 41, 51, 72, 91, 92))

# Tạo list các bộ dữ liệu đã đọc vào
list_of_hh_data <- list(hh2, hh3, hh4, hh5, hh6)
list_of_ch_data <- list(ch2, ch3, ch4, ch5, ch6)
list_of_maps <- list(map_mics2, map_mics3, map_mics4, map_mics5, map_mics6)
list_of_years <- c(2000, 2006, 2011, 2014, 2021)

# Áp dụng hàm xử lý cho tất cả các năm và gộp lại thành một bảng duy nhất
all_diarrhea_data <- pmap_dfr(
  list(list_of_hh_data, list_of_ch_data, list_of_years, list_of_maps),
  process_mics_data
)

# --- Bước 2: Tính toán tỷ lệ tiêu chảy theo năm và nhóm nguồn nước ---
diarrhea_trends <- all_diarrhea_data %>%
  na.omit() %>%
  group_by(year, improved_water) %>%
  summarise(
    diarrhea_rate = mean(has_diarrhea, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    water_status = ifelse(improved_water == 1, "Nước sạch", "Nước không sạch")
  )

cat("\n**Bảng tổng hợp Tỷ lệ tiêu chảy theo năm và chất lượng nước**\n")
print(diarrhea_trends)


# --- Bước 3: Trực quan hóa chuỗi thời gian ---
ggplot(diarrhea_trends, aes(x = year, y = diarrhea_rate, color = water_status, group = water_status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Mô hình xu hướng Tỷ lệ Tiêu chảy ở Trẻ em theo Chất lượng Nguồn nước (2000-2021)",
    subtitle = "So sánh giữa các hộ gia đình sử dụng nước sạch và nước không sạch",
    x = "Năm",
    y = "Tỷ lệ Trẻ em bị Tiêu chảy (%)",
    color = "Chất lượng Nguồn nước"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")