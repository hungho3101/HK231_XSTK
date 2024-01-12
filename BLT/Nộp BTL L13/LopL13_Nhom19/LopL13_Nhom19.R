library(tidyverse)
library(data.table)
library(caret)
library(kernlab)
library(ggplot2)
library(ResourceSelection)

wp<-read.csv("~/water_potability.csv")
head(wp,10) #xem dữ liệu của 10 dòng đầu tiên

#Số gia trị và tỉ lệ NA
wp %>% gather(key = "variable", value = "value") %>%
  group_by(variable) %>% summarise(na_num = sum(is.na(value)),na_per=mean(is.na(value)))

#thay các giá trị khuyết bằng giá trị trung bình
wp$ph[is.na(wp$ph)]=mean(wp$ph,na.rm=T) 
wp$Sulfate[is.na(wp$Sulfate)]=mean(wp$Sulfate,na.rm=T)
wp$Trihalomethanes[is.na(wp$Trihalomethanes)]=mean(wp$Trihalomethanes,na.rm=T)

#Kiểm tra lại các giá trị 
wp %>% gather(key = "variable", value = "value") %>%
  group_by(variable) %>% summarise(na_num = sum(is.na(value)),na_per=mean(is.na(value)))

#Xử lí biến phân loại "Potability"
as.factor(wp$Potability)

#Chia dữ liệu
y <- wp$Potability
set.seed(42, sample.kind = "default")
test_index <- createDataPartition(y=wp$Potability, times=1, p=0.2, list=F)
test_data <- wp[test_index,]
train_dada <- wp[-test_index,]
 
#làm sạch dữ liệu
apply(is.na(wp),2,sum)   #thống kê số lượng dữ liệu khuyết
apply(is.na(wp),2,which) #xuất vị trí dòng chứa dữ liệu khuyết
apply(is.na(wp),2,mean)  #tính tỉ lệ dữ liệu khuyết
table(wp$Potability, dnn="Potability")

#Đồ thị boxplot
gather(wp, key = "variable", value = "value", -Potability) %>%
  ggplot(aes(x=factor(Potability), y=value, col = Potability)) +
  geom_boxplot() + 
  facet_wrap(~ variable, scales="free") +
  xlab("Potability") +
  ylab("Values in different units of measure")

boxplot(wp$ph~wp$Potability,col=c("red","gray"),xlab="Potability",ylab="ph",main="pH and Potability")

boxplot(wp$Hardness~wp$Potability,col=c("coral","gray"),xlab="Potability",ylab="Hardness",main="Hardness and Potability")

boxplot(wp$Solids~wp$Potability,col=c("orange","gray"),xlab="Potability",ylab="Solids",main="Solids and Potability")

boxplot(wp$Chloramines~wp$Potability,col=c("yellow","gray"),xlab="Potability",ylab="Chloramines",main="Chloramines and Potability")

boxplot(wp$Sulfate~wp$Potability,col=c("green","gray"),xlab="Potability",ylab="Sulfate",main="Sulfate and Potability")

boxplot(wp$Conductivity~wp$Potability,col=c("lightblue","gray"),xlab="Potability",ylab="Conductivity",main="Conductivity and Potability")

boxplot(wp$Organic_carbon~wp$Potability,col=c("blue","gray"),xlab="Potability",ylab="Organic_carbon",main="Organic_carbon and Potability")

boxplot(wp$Trihalomethanes~wp$Potability,col=c("violet","gray"),xlab="Potability",ylab="Trihalomethanes",main="Trihalomethanes and Potability")

boxplot(wp$Turbidity~wp$Potability,col=c("pink","gray"),xlab="Potability",ylab="Turbidity",main="Turbidity and Potability")

#Đồ thị Histogram 
gather(wp,key = "variable", value = "value") %>%
  ggplot(aes(value, fill = variable)) + geom_histogram() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ variable, scales = "free")

hist(wp$ph,main="Histogram of pH",col="red",xlab="pH",ylab="Count",ylim=c(0,1200),labels=T)

hist(wp$Hardness,main="Histogram of Hardness",col="coral",xlab="Hardness",ylab="Count",ylim=c(0,1000),labels=T)

hist(wp$Solids,main="Histogram of Solids",col="orange",xlab="Solids",ylab="Count",ylim=c(0,800),labels=T)

hist(wp$Chloramines,main="Histogram of Chloramines",col="yellow",xlab="Chloramines",ylab="Count",ylim=c(0,1000),labels=T)

hist(wp$Sulfate,main="Histogram of Sulfate",col="green",xlab="Sulfate",ylab="Count",ylim=c(0,1400),labels=T)

hist(wp$Conductivity,main="Histogram of Conductivity",col="lightblue",xlab="Conductivity",ylab="Count",ylim=c(0,800),labels=T)

hist(wp$Organic_carbon,main="Histogram of Organic_carbon",col="blue",xlab="Organic_carbon",ylab="Count",ylim=c(0,800),labels=T)

hist(wp$Trihalomethanes,main="Histogram of Trihalomethanes",col="violet",xlab="Trihalomethanes",ylab="Count",ylim=c(0,1000),labels=T)

hist(wp$Turbidity,main="Histogram of Turbidity",col="pink",xlab="Turbidity",ylab="Count",ylim=c(0,1000),labels=T)

#Đồ thị Barplot 
barplot(table(wp$Potability),col=c("lightblue","blue"),xlab="Potability", ylab="Count",ylim=c(0,2000),main="Potability")

#Kiểm tra tương quan 
y <- wp$Potability
set.seed(42, sample.kind = "default")
test_index <- createDataPartition(y, times=1, p=0.2, list=F)
train_data <- wp[-test_index,]
test_data <- wp[test_index,]
cor_mat <- cor(train_data, use = "complete.obs")
library(corrplot)
corrplot(cor_mat, type = "upper",
         tl.col = "black", tl.srt = 45, method="color")

#Mô hình hồi quy logistic 
glm<-glm(Potability ~ ph+Hardness+Solids+Chloramines+Sulfate+Conductivity+
           Organic_carbon+Trihalomethanes+Turbidity, data= train_data)
summary(glm)

#Độ chính xác của mô hình hồi quy Logistic
set.seed(42, sample.kind = "default")
train_glm <- train(factor(Potability) ~., method = "glm",
                   data = train_data_1)
pred_glm <- predict(train_glm, test_data_1)
confusionMatrix(pred_glm,
                factor(test_data_1$Potability))$overall["Accuracy"]

#Kiểm tra giả định Hosmer and Lemeshow 
library(generalhoslem)
hl <- hoslem.test(glm $fitted.values,fitted(glm ))
hl 

#knn 
set.seed(42, sample.kind = "default")
train_knn <- train(factor(Potability) ~ ., method = "knn",
                   tuneGrid = data.frame(k = seq(3, 45, 2)),
                   data = test_data_1,
                  trControl= train_control)
pred_knn <- predict(train_knn, test_data_1)
confusionMatrix(pred_knn,
                factor(test_data_1$Potability))$overall["Accuracy"]

#naivebayes
library(naivebayes)
set.seed(42, sample.kind = "default")
train_nb <- train(factor(Potability) ~., method = "naive_bayes",
                  data = test_data_1)
pred_nb <- predict(train_nb, test_data_1)
confusionMatrix(pred_nb,
                factor(test_data_1$Potability))$overall["Accuracy"]

#QDA
set.seed(42, sample.kind = "default")
train_qda <- train(factor(Potability) ~., data = test_data_1,
                   method = "qda")
pred_qda <- predict(train_qda, test_data_1)
confusionMatrix(pred_qda,
                factor(test_data_1$Potability))$overall["Accuracy"]

#SVM 
set.seed(42, sample.kind = "default")
train_svm <- train(factor(Potability) ~., method = "lssvmRadial",
                   data = test_data_1,
                   tuneGrid = expand.grid(
                     tau = c(0.001,0.009,0.01,0.1),
                     sigma = c(0.001,0.009,0.01,0.1)),
                   trControl = train_control)
pred_svm <- predict(train_svm, test_data_1)
confusionMatrix(pred_svm,
                factor(test_data_1$Potability))$overall["Accuracy"]






