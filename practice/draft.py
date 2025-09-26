import sys
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

sys.stdout.reconfigure(encoding='utf-8')
df = pd.read_csv('e:/regression-analysis/data/data1.csv',index_col=False)

"""
- Giới tính (Nam/Nữ) – biến định tính
- Tuổi – biến định lượng
- Thu nhập (Nghèo, Trung bình, Giàu có) – biến định tính
- BMI – biến định lượng
- Địa điểm sinh sống (Thành thị/Nông thôn) – biến định tính
- Tiểu đường (Có/Không) – biến định tính
- Tim mạch (Có/Không) – biến định tính
- Gãy xương hông (Có/Không) – biến định tính
- Thời gian nằm viện (ngày) – biến định lượng (biến phụ thuộc chính trong hồi quy tuyến tính)
"""

#-----------Thống kê cơ bản--------------
print("Thống kê mô tả (biến định lượng):")
print(df[["Tuổi", "BMI", "Thời gian nằm viện (ngày)"]].describe())

# Tần suất cho biến định tính
print("\nTần suất biến định tính:")
categorical_vars = ["Giới tính", "Thu nhập", "Địa điểm sinh sống", 
                    "Tiểu đường", "Tim mạch", "Gãy xương hông"]
for var in categorical_vars:
    print(f"\n{df[var].value_counts()}")
    
    
# Trực quan hóa
sns.histplot(df["Tuổi"], kde=True)
plt.title("Phân bố tuổi bệnh nhân")
plt.show()

sns.histplot(df["BMI"], kde=True)
plt.title("Phân bố BMI")
plt.show()

sns.boxplot(x="Giới tính", y="Thời gian nằm viện (ngày)", data=df)
plt.title("So sánh thời gian nằm viện theo giới tính")
plt.show()


#-----------Mô hình hồi quy tuyến tính--------------
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score

X = df[["Tuổi"]]
y = df["Thời gian nằm viện (ngày)"]

model_simple = LinearRegression()
model_simple.fit(X, y)

y_pred= model_simple.predict(X)
print("\nHệ số hồi quy (slope):", model_simple.coef_[0])
print("R^2 score:", r2_score(y, y_pred))


plt.figure(figsize=(8,5))
plt.scatter(X, y, color='blue', label='Dữ liệu thực tế')
plt.plot(X, y_pred, color='red', linewidth=2, label='Đường hồi quy')
plt.xlabel("Tuổi")
plt.ylabel("Thời gian nằm viện (ngày)")
plt.title("Hồi quy tuyến tính: Tuổi vs Thời gian nằm viện")
plt.legend()
plt.show()

