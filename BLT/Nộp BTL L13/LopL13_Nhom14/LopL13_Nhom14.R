library(ggplot2)
library(dplyr)
library(plotly)
library(cowplot)
library(caret)
library(vcd)
library(ResourceSelection)
library(pROC)
library(corrplot)
library(keras)
library(tensorflow)
library(xgboost)
library(class)
water_potability <- read.csv("D:/AQ/Probability and Statistics/water_potability.csv")
head(water_potability)
sum(is.na(water_potability))
summary(is.na(water_potability))
r_na <- colSums(is.na(water_potability))/3276
print(r_na)
water_potability_no_na<- na.omit(water_potability)
head(water_potability_no_na)
summary(water_potability_no_na)

water_potability_no_na$Potability <- as.factor(water_potability_no_na$Potability)


#Ve boxplot
summary<-boxplot(water_potability_no_na$pH~water_potability_no_na$Potability, xlab = "Potability", ylab = "pH")$stats
colnames(summary)<-c("0","1")
rownames(summary)<-c("Min", "First Quartile", "Median", "Third Quartile", "Maximum")
summary

summary<-boxplot(water_potability_no_na$Hardness~water_potability_no_na$Potability, xlab = "Potability", ylab = "Hardness")$stats
colnames(summary)<-c("0","1")
rownames(summary)<-c("Min", "First Quartile", "Median", "Third Quartile", "Maximum")
summary

summary<-boxplot(water_potability_no_na$Solids~water_potability_no_na$Potability, xlab = "Potability", ylab = "Solids")$stats
colnames(summary)<-c("0","1")
rownames(summary)<-c("Min", "First Quartile", "Median", "Third Quartile", "Maximum")
summary

summary<-boxplot(water_potability_no_na$Chloramines~water_potability_no_na$Potability, xlab = "Potability", ylab = "Chloramines")$stats
colnames(summary)<-c("0","1")
rownames(summary)<-c("Min", "First Quartile", "Median", "Third Quartile", "Maximum")
summary

summary<-boxplot(water_potability_no_na$Sulfate~water_potability_no_na$Potability, xlab = "Potability", ylab = "Sulfate")$stats
colnames(summary)<-c("0","1")
rownames(summary)<-c("Min", "First Quartile", "Median", "Third Quartile", "Maximum")
summary

summary<-boxplot(water_potability_no_na$Conductivity~water_potability_no_na$Potability, xlab = "Potability", ylab = "Conductivity")$stats
colnames(summary)<-c("0","1")
rownames(summary)<-c("Min", "First Quartile", "Median", "Third Quartile", "Maximum")
summary

summary<-boxplot(water_potability_no_na$Organic_carbon~water_potability_no_na$Potability, xlab = "Potability", ylab = "Organic_carbon")$stats
colnames(summary)<-c("0","1")
rownames(summary)<-c("Min", "First Quartile", "Median", "Third Quartile", "Maximum")
summary

summary<-boxplot(water_potability_no_na$Trihalomethanes~water_potability_no_na$Potability, xlab = "Potability", ylab = "Trihalomethanes")$stats
colnames(summary)<-c("0","1")
rownames(summary)<-c("Min", "First Quartile", "Median", "Third Quartile", "Maximum")
summary

summary<-boxplot(water_potability_no_na$Turbidity~water_potability_no_na$Potability, xlab = "Potability", ylab = "Turbidity")$stats
colnames(summary)<-c("0","1")
rownames(summary)<-c("Min", "First Quartile", "Median", "Third Quartile", "Maximum")
summary

#Ve hist
hist(water_potability_no_na$pH, xlab = "pH", ylab = "frequency", main = "Biểu đồ pH")
hist(water_potability_no_na$Hardness, xlab = "mg/L", ylab = "frequency", main = "Biểu đồ Hardness")
hist(water_potability_no_na$Solids, xlab = "ppm", ylab = "frequency", main = "Biểu đồ Solids")
hist(water_potability_no_na$Chloramines, xlab = "ppm", ylab = "frequency", main = "Biểu đồ Chloramines")
hist(water_potability_no_na$Sulfate, xlab = "mg/L", ylab = "frequency", main = "Biểu đồ Sulfate")
hist(water_potability_no_na$Conductivity, xlab = "μS/cm", ylab = "frequency", main = "Biểu đồ Conductivity")
hist(water_potability_no_na$Organic_carbon, xlab = "ppm", ylab = "frequency", main = "Biểu đồ Organic Carbon")
hist(water_potability_no_na$Trihalomethanes, xlab = "μg/L", ylab = "frequency", main = "Biểu đồ Trihalomehthanes")
hist(water_potability_no_na$Turbidity, xlab = "NTU", ylab = "frequency", main = "Biểu đồ Turbidity")

#Moi quan he tuong quan
newwater_potability <- sapply(water_potability_no_na,as.numeric)
cor_matrix <- cor(newwater_potability)
cor_matrix
corrplot(cor_matrix, method = "circle")

#Xay dung mo hinh
set.seed(5)
water_potability_no_na <- water_potability_no_na[sample(nrow(water_potability_no_na)), ]
trainIndex <- createDataPartition(water_potability_no_na$Potability, p = 0.7, list = FALSE)
trainData <- water_potability_no_na[trainIndex, ]
testData <- water_potability_no_na[-trainIndex, ]
row.names(trainData) <- NULL
rownames(trainData) <- 1:nrow(trainData)
row.names(testData) <- NULL
rownames(testData) <- 1:nrow(testData)
head(testData)

logistic <- glm(factor(Potability)~pH+Hardness+Solids+Chloramines+Sulfate+Conductivity+Organic_carbon+Trihalomethanes+Turbidity, data = trainData, family = binomial)
summary(logistic)

predicted.classes1 <- ifelse(predict(logistic, newdata = trainData, type = "response") >0.5, "0", "1")
confusionMatrix(factor(predicted.classes1, levels = c(0,1)), factor(trainData$Potability, levels = c(0,1)))

predicted.classes <- ifelse(predict(logistic, newdata = testData, type = "response") >0.5, "0", "1")
confusionMatrix(factor(predicted.classes, levels = c(0,1)), factor(testData$Potability, levels = c(0,1)))

threshold_test <- predict(logistic, newdata = testData, type = "response")
roc_obj <- roc(testData$Potability, threshold_test)
plot(roc_obj, main = "ROC curve")
coords <- coords(roc_obj, "best", ret=c("threshold", "specificity", "sensitivity"))
best_point <- coords[[1]]
best_point

predicted.classes1 <- ifelse(predict(logistic, newdata = trainData, type = "response") > best_point, "1", "0")
confusionMatrix(factor(predicted.classes1,levels = c(0,1)), factor(trainData$Potability, levels= c(0,1)))

predicted.classes <- ifelse(predict(logistic, newdata = testData, type = "response") > best_point, "1", "0")
confusionMatrix(factor(predicted.classes,levels = c(0,1)), factor(testData$Potability, levels= c(0,1)))


fitted_numeric <- as.numeric(as.character(logistic$fitted.values))
potability_int <- as.integer(as.character(trainData$Potability))
hl <- hoslem.test(potability_int, fitted_numeric, g=10)
hl
