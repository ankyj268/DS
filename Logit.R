
#####bankData

dt<- read.csv(file.choose(),header = T, stringsAsFactors = F)
str(dt)                
dim(dt)
names(dt)

head(dt,5)

#check count of unique values in each column

sapply(dt,function(x)length(unique(x)))

summary(dt)

#check count of NA's values in each column

sapply(dt,function(x)sum(is.na(x)))

install.packages("Hmisc")
library(Hmisc)

#replace missing values with the mean
dt$age<- as.numeric(impute(dt$age,mean))
dt$balance<- as.numeric(impute(dt$balance,mean))

#check outliers
boxplot(dt$exclaim_mess)

barplot(dt$exclaim_mess)

boxplot(dt$balance)

#replace the outliers

bench<- 33 - 1.5*IQR(dt$age)
bench
dt$age[dt$age<bench] <- bench

bench<- 72 - 1.5*IQR(dt$balance)
bench
dt$balance[dt$balance<bench] <- bench

summary(dt$age)

#############Logit
dt$y<- ifelse(dt$y=="no",0,1)

####check corelation
install.packages("GGally")
library(GGally)
ggpairs(data = dt[,sapply(dt, is.numeric)],title = "Bank Data")
correlation_matrix<-cor(dt[,sapply(dt,is.numeric)])
write.csv(correlation_matrix,"correlation.csv")


##############Check multicollinearity
mc<- lm(spam~.,data = dt)
install.packages("car")
library(car)
summary(mc)
vif(mc)



#####################creating dummy variables
install.packages("caret")
library(caret)
dmy<- dummyVars("~job+marital+education+poutcome+contact+month", data = dt,fullRank = T)
dt_trans<- data.frame(predict(dmy,newdata=dt))
summary(dt_trans)
final_data<-cbind(dt,dt_trans)
str(final_data)
library(dplyr)

######deleting redundunt columns

final_data<-select(final_data,-c(job,marital,contact,education,poutcome,month))

#######sampling
SampleSize<- floor(0.75 * nrow(final_data)) 
#set.seed(1234)
TD<- sample(seq_len(nrow(final_data)),size=SampleSize)
Train<-final_data[TD,]
Test<- final_data[-TD,]
dim(Train)
dim(Test)


#################### Logistic regression
head(Train)
model<- glm(y~.,data = Train, family = "binomial")
summary(model)
#pred1<-predict(model1,data.frame(hp=150),type = "response")
model<- glm(y~age+jobentrepreneur+jobhousemaid+maritalmarried+balance+campaign,data = final_data, family = "binomial")
summary(model)

pred<- predict(model,type = "response")
head(pred)
actualPred<- (cbind(pred,final_data))
#head(actualPred)
View(actualPred)
summary(actualPred)
write.csv(actualPred,"Predictions.csv")

#############confusion matrix
install.packages("e1071")
library(e1071)
actualPred$pred<- ifelse(actualPred$pred>0.5,1,0)

head(actualPred)
confmatrix<- confusionMatrix(as.factor(actualPred$pred),as.factor(actualPred$y))
head(actualPred)
confmatrix

mean(actualPred$pred!=actualPred$y)

??ROCR

install.packages("ROCR")
library(ROCR)
logit_scores <- prediction(predictions=actualPred$pred, labels=actualPred$y)
logit_perf <- performance(logit_scores, "tpr", "fpr")
plot(logit_perf,col = "darkblue",lwd=2,xaxs="i",yaxs="i",tck=NA, main="ROC Curve")
box()
abline(0,1, lty = 300, col = "green")
grid(col="aquamarine")
logit_auc <- performance(logit_scores, "auc")
as.numeric(logit_auc@y.values) 



data1<- cbind(dt,pred)
head(data1)
setwd("G:/../data")
write.csv(actualPred,"Predictions.csv",sep = ",")
summary(model)
