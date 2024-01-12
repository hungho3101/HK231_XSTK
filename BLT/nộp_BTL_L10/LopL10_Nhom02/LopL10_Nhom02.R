#Đọc dữ liệu
library(readr)
data <- read.csv("~/BTLxstk/data.csv")
View(data)

#chuyển đổi 2 cột infill_pattern và cột material thành biến phân loại 
data$infill_pattern <- factor(data$infill_pattern)
data$material <- factor(data$material)
head(data)

#Thống kê mô tả biến liên tục 
summary(data)
#tính độ lệch chuẩn 
selected_columns <- c("layer_height", "wall_thickness", "infill_density", "nozzle_temperature", 
    "bed_temperature", "print_speed", "fan_speed", "roughness", "tension_strenght", "elongation")
dt <- data[, selected_columns]
sd_values<- sapply(dt, sd)
result <- data.frame(Variable = names(sd_values), Standard_Deviation = sd_values)
print(result)

#thống kê mô tả biến phân loại 
categorical_variables <- c("material", "infill_pattern")
# Tạo và in bảng thống kê
for (variable in categorical_variables) {
  cat("Bảng thống kê cho biến", variable, ":\n")
  print(table(data[[variable]]))
  cat("\n")
}

# Vẽ histogram 
par(mfrow = c(1, 3))
# Vẽ histogram cho biến elongation
hist(data$elongation,
     main = "Histogram của Elongation",
     xlab = "Elongation",
     ylab = "Tần suất",
     col = "lightblue",
     border = "black")
# Vẽ histogram cho biến tension_strenght
hist(data$tension_strenght,
     main = "Histogram của Tension Strenght",
     xlab = "Tension Strenght",
     ylab = "Tần suất",
     col = "lightgreen",
     border = "black")
# Vẽ histogram cho biến roughness
hist(data$roughness,
     main = "Histogram của Roughness",
     xlab = "Roughness",
     ylab = "Tần suất",
     col = "orange",
     border = "black")

# Biểu đồ boxplot
boxplot(data$roughness~data$infill_pattern)
boxplot(data$roughness~data$material)
boxplot(data$tension_strenght~data$infill_pattern)
boxplot(data$tension_strenght~data$material)
boxplot(data$elongation~data$infill_pattern)
boxplot(data$elongation~data$material)

#vẽ biểu đồ phân tán 
ggplot(data, aes(x =  fan_speed, y = elongation)) +
  geom_point() +
  labs(title = "Scatter Plot of elongation vs fan_speed",
       x = "  fan_speed",
       y = "elongation")

#vẽ ma trận tương quan 
install.packages("corrplot")
# Sau khi cài đặt, load gói vào R
library(corrplot)
# Lọc ra các cột bạn quan tâm
selected_columns <- c("layer_height", "wall_thickness", "infill_density", "nozzle_temperature", "bed_temperature", "print_speed", "fan_speed", "roughness", "tension_strenght", "elongation")
dt <- data[, selected_columns]
# Tính ma trận tương quan
M<- cor(dt)
# Sử dụng corrplot để vẽ ma trận tương quan
corrplot(M, method = "number", number.cex = 0.5)

#xây dựng mô hình hồi quy 1 cho biến roughness
dabien<-lm(data$roughness~data$layer_height+data$wall_thickness+data$infill_density+data$infill_pattern+data$nozzle_temperature+data$bed_temperature+data$print_speed+data$material)
summary(dabien)
fitted(dabien)
resid(dabien)
vephandu<-par(mfrow=c(2,2))
plot(dabien)

#xây dựng mô hình hồi quy 2 cho biến rooughness ( khi lược bỏ wall_thickness; infill_density và infill_pattern)
dabien2<-lm(data$roughness~data$layer_height+data$nozzle_temperature+data$bed_temperature+data$print_speed+data$material)
summary(dabien2)
fitted(dabien2)
resid(dabien2)
vephandu<-par(mfrow=c(2,2))
plot(dabien2)

#so sánh hiệu quả của 2 mô hình. 
anova(dabien,dabien2)


