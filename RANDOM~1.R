#Libraries
library(caTools)
library (caret)   
library(lattice)
library(ggplot2)
library(ROSE)
library(smotefamily)
library(pROC)
library(randomForest)  
library(e1071)  
library(caret)
library(MLmetrics)
####################
#######
creditcard <- read.csv("C:/Users/Gulsev/Desktop/ACM514 FINAL PROJECT/creditcard.csv",nrows = 50000, header=TRUE)
View(creditcard)
nrow(creditcard)
###################################
creditcard$Class<-factor(creditcard$Class,levels=c(0,1))
#########Test&Train Partition#########
set.seed(123)
data_sample=sample.split(creditcard$Class,SplitRatio = 0.80)

train_data=subset(creditcard,data_sample==TRUE)
#227846(0) 31(1)
test_data=subset(creditcard,data_sample==FALSE)
#56961 (0) 31(1)
dim(train_data) #40000    31
dim(test_data)  #10000    31
table(train_data$Class)  
#   0     1 
# 39882   118 
##########################################################################

# Model- 1: Randoom Forest Regression Modeling with Random Under- Sampling(RUS)Method

#########################################################################

# Model-1.1- Random Under- Sampling(RUS)Method
table(train_data$Class)
#Result:39882(0) 118(1) 
no_fraud<-118
new_frac_fraud<-0.50
new_n_total<-no_fraud/new_frac_fraud

undersampling_result<-ovun.sample(Class~.,data=train_data,method="under",N=new_n_total,seed=123)

undersampled_credit<-undersampling_result$data
table(undersampled_credit$Class)
#Result
#0   1 
#118 118
#Model-1.2 randomForest:: with RUS Method

####################

set.seed(421)
model_rf <- randomForest(Class~., undersampled_credit, importance = T, nPerm = 4, type = "classification", ntree = 775, mtry = 1)
pred_rf <- predict(model_rf, test_data[,-31], type = "class")

length(test_data$Class)
length(pred_rf)
levels(test_data$Class)
levels(pred_rf)
conf_table_rf <- confusionMatrix(test_data$Class,  pred_rf , mode = "everything", positive = "1")

conf_table_rf
plot(model_rf)
####################33
roc.curve(test_data$Class,pred_rf, plotit = TRUE)

#F1_Score(pred_rf,test_data$Class,positive = NULL)
#plot(margin(model_rf,test_data$Class))
varImpPlot(model_rf)
#########################################
