library(corrplot)
library(forecast)
library(ggplot2)
library(rpart)
library(rpart.plot)
#Đọc file dữ liệu
XSTK<- read.csv("D:/Xác suất thống kê/water_potability.csv")
XSTK 
head(XSTK,10)
str(XSTK) 
#Tổng quan các biến
summary(XSTK[1:9])

#Tên các biến
names(XSTK) 

#Bảng tần suất của biến phân loại
table(XSTK$Potability)

#Tổng, tỉ lệ, vị trí của các giá trị khuyết
apply(is.na(XSTK),2,sum)
apply(is.na(XSTK),2,which)
apply(is.na(XSTK),2,mean)

#Xử lí dữ liệu khuyết bằng giá trị trung bình
ph1<-mean(XSTK[XSTK$Potability == 0 & XSTK$Hardness <= 150, "ph"],na.rm=T)
ph1
ph2<-mean(XSTK[XSTK$Potability == 0 & XSTK$Hardness > 150, "ph"],na.rm=T)
ph2
ph3<-mean(XSTK[XSTK$Potability == 1 & XSTK$Hardness <= 150, "ph"],na.rm=T)
ph3
ph4<-mean(XSTK[XSTK$Potability == 1 & XSTK$Hardness > 150, "ph"],na.rm=T)
ph4
XSTK[ XSTK$Potability == 0 & XSTK$Hardness <= 150 & is.na(XSTK$ph), "ph"] <- ph1
XSTK[ XSTK$Potability == 0 & XSTK$Hardness > 150 & is.na(XSTK$ph), "ph"] <- ph2
XSTK[ XSTK$Potability == 1 & XSTK$Hardness <= 150 & is.na(XSTK$ph), "ph"] <- ph3
XSTK[ XSTK$Potability == 1 & XSTK$Hardness > 150 & is.na(XSTK$ph), "ph"] <- ph4
XSTK$Sulfate[is.na(XSTK$Sulfate)]=mean(XSTK$Sulfate,na.rm=T)
XSTK$Trihalomethanes[is.na(XSTK$Trihalomethanes)]=mean(XSTK$Trihalomethanes,na.rm=T)

#Kiểm tra đã xử lí dữ liệu khuyết chưa
apply(is.na(XSTK),2,which)

#Xử lí dữ liệu bằng biểu đồ Hist
par(mfrow=c(1,3))
hist(XSTK$ph,xlab="pH",main="Histogram of pH",col="green",labels=T,ylim=c(0,1200))
hist(XSTK$Hardness,xlab="Hardness",main="Histogram of Hardness",col="green",labels=T,ylim=c(0,1000))
hist(XSTK$Solids,xlab="Solids",main="Histogram of Solids",col="green",labels=T,ylim=c(0,800))
hist(XSTK$Chloramines,xlab="Chloramines",main="Histogram of Chloramines",col="green",labels=T,ylim=c(0,1000))
hist(XSTK$Sulfate,xlab="Sulfate",main="Histogram of Sulfate",col="green",labels=T,ylim=c(0,1400))
hist(XSTK$Conductivity,xlab="Conductivity",main="Histogram of Conductivity",col="green",labels=T,ylim=c(0,800))
hist(XSTK$Organic_carbon,xlab="Organic_carbon",main="Histogram of Organic_carbon",col="green",labels=T,ylim=c(0,800))
hist(XSTK$Trihalomethanes,xlab="Trihalomethanes",main="Histogram of Trihalomethanes",col="green",labels=T,ylim=c(0,1000))
hist(XSTK$Turbidity,xlab="Turbidity",main="Histogram of Turbidity",col="green",labels=T,ylim=c(0,1000))

#Biểu đồ tần suất của bảng Potability
barplot(table(XSTK$Potability),col=c("grey","lightblue"),xlab="Potability", ylab="Frequency",ylim=c(0,2000),main="Barplot of Potability")

#So sánh số liệu của biến Potability với những biến còn lại 
boxplot(XSTK$ph~XSTK$Potability,horizontal = TRUE ,col=c("grey","lightblue"),xlab="Potability",ylab="ph",main="pH and Potability")
boxplot(XSTK$Hardness~XSTK$Potability,horizontal = TRUE ,col=c("grey","lightblue"),xlab="Potability",ylab="Hardness",main="Hardness and Potability")
boxplot(XSTK$Chloramines~XSTK$Potability,horizontal = TRUE ,col=c("grey","lightblue"),xlab="Potability",ylab="Chloramines",
        main="Chloramines and Potability")
boxplot(XSTK$Solids~XSTK$Potability,horizontal = TRUE ,col=c("grey","lightblue"),xlab="Potability",ylab="Solids",main="Solids and Potability")
boxplot(XSTK$Turbidity~XSTK$Potability,horizontal = TRUE ,col=c("grey","lightblue"),xlab="Potability",ylab="Turbidity",main="Turbidity and Potability")
boxplot(XSTK$Organic_carbon~XSTK$Potability,horizontal = TRUE ,col=c("grey","lightblue"),xlab="Potability",ylab="Organic_carbon",
        main="Organic_carbon and Potability")
boxplot(XSTK$Sulfate~XSTK$Potability, horizontal = TRUE ,col=c("grey","lightblue"),xlab="Potability",ylab="Sulfate",main="Sulfate and Potability")
boxplot(XSTK$Trihalomethanes~XSTK$Potability,horizontal = TRUE ,col=c("grey","lightblue"),xlab="Potability",ylab="Trihalomethanes",
        main="Trihalomethanes and Potability")
boxplot(XSTK$Conductivity~XSTK$Potability,horizontal = TRUE ,col=c("grey","lightblue"),xlab="Potability",ylab="Conductivity",
        main="Conductivity and Potability")

#Lập bảng ma trận tương quan giữa các biến và vẽ ma trận tương quan
round(cor(XSTK[,1:9]),3)
M=cor(XSTK[,1:9])
corrplot(M, method="square")

#Chia tập dữ liệu ra thành 2 phần: train-data và test-data
set.seed(8)
train.rows <- sample(rownames(XSTK), dim(XSTK)[1]*0.8)
traindata <- XSTK[train.rows, ]
test.rows <- setdiff(rownames(XSTK), train.rows)
testdata <- XSTK[test.rows, ]

#Mô hình hồi quy logistic
model <- glm(Potability~., family="binomial", data=traindata)
research<-step(model)
summary(research)

#Độ tin cậy 95% 
confint(research)
#Tìm khoảng tin cậy cho tỷ lệ chênh lệch
exp(cbind(OR = coef(research), confint(research)))

#Đưa ra dự đoán dựa trên test_data
predicted <- predict(research,testdata, type="response")
testdata$predicted<-round(predicted)
head(testdata,10)

#Lập bảng so sánh
Observation<-table(testdata$Potability)
Predicted_value<-table(testdata$predicted)
rbind(Observation,Predicted_value)
#Kiểm tra tỷ lệ phân loại sai của các quan sát và dự đoán
accuracy(testdata$Potability, testdata$predicted)

#Mô hình Decision Tree
fit <- rpart(Potability~., data = traindata, method = 'class')
rpart.plot(fit, extra = 106)

#Tạo dự đoán
testdata$predicttree <-predict(fit, testdata, type = 'class')
head(testdata,10)
#Tạo bảng so sánh
PredictedTree<-table(testdata$predicttree)
table<- rbind(Observation,PredictedTree)
table
#chuyển kết quả sang định dạng số
testdata$predicttree<-as.numeric(as.character(testdata$predicttree))
#tỷ lệ phân loại sai của các quan sát và dự đoán
accuracy(testdata$Potability, testdata$predicttree)


