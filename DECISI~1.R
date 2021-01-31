#Decision Tree Algortihm

#Libraries
library(caTools)
library(rpart)
library(rpart.plot)
library(MLmetrics)
library (caret)   
library(lattice)
library(ggplot2)
library(ROSE)
library(smotefamily)
###########################
creditcard <- read.csv("C:/Users/Gulsev/Desktop/ACM514 FINAL PROJECT/creditcard.csv",nrows = 50000, header=TRUE)
View(creditcard)
nrow(creditcard)
###################################
creditcard$Class<-factor(creditcard$Class,levels=c(0,1))
#########Test&Train Partition#########
set.seed(123)
data_sample=sample.split(creditcard$Class,SplitRatio = 0.8)

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

# Model- 1: Decision TREE Modeling with Random Under- Sampling(RUS)Method

#########################################################################

#Model-1.1-Random Under- Sampling(RUS) Method Implementation

table(train_data$Class)
#Result:227452(0) 394(1) 
no_fraud<-118
new_frac_fraud<-0.50
new_n_total<-no_fraud/new_frac_fraud

undersampling_result<-ovun.sample(Class~.,data=train_data,method="under",N=new_n_total,seed=123)

undersampled_credit<-undersampling_result$data

table(undersampled_credit$Class)
#Result
#0   1 
#118 

#MODELING-1.2 Decision Tree with Under Sampling Method  


decisionTree_model <- rpart(Class ~ . ,undersampled_credit, method = 'class')
predicted_value <- predict(decisionTree_model,test_data[,-31], type = 'class')
rpart.plot(decisionTree_model,extra="auto",type=2,tweak=1)
#burada söyle bir durum var decisionTree_model'i eger credit_smote_result ile denersem daha düsük çikiyor accuracy ve agaç dallanmiyor 
#test_data ile deneyinde daha iyi çikiyor souç bu iki ayri durumun kodunu rapora ekle ve aciklamasini yap 
#credit_smote_result
summary(decisionTree_model)
#-------
  #Performance Evoluation for Decision Tree for RUS
predicted_value<-predict(decisionTree_model,test_data[,-31],type='class')
confusionMatrix(predicted_value,test_data$Class,dnn = c("Predictions", "Actual Values"),positive = "1")
F1_Score(predicted_value,test_data$Class,positive = NULL)
##########################################################################

# Model- 2: Decision TREE Modeling with SMOTE Method

#########################################################################

#Model-2.1-SMOTE Method
# Smote to Balance The Dataset
table(train_data$Class)
#Result:39882(0) 118 (1) 

n0<-39882 #= legit cases
n1<-118   #=fraud cases
r0<-0.6
ntimes<-((1-r0)/r0)*(n0/n1)-1
smote_output=SMOTE(X=train_data[,-c(1,31)],target=train_data$Class,K=3,dup_size=ntimes)

credit_smote_result<-smote_output$data

colnames(credit_smote_result)[30]<-"Class"
prop.table(table(credit_smote_result$Class))

#Smote result :0.6003432(0) 0.3996568 (1)

#Model-2.2 -Decision Tree Modeling with Smote Method 

decisionTree_model <- rpart(Class ~ . ,credit_smote_result, method = 'class')
predicted_value <- predict(decisionTree_model,test_data[,-31], type = 'class')
rpart.plot(decisionTree_model,extra="auto",type=2,tweak=0.80)
summary(decisionTree_model)

#Performance Evoluation for Decision Tree for Smote
predicted_value<-predict(decisionTree_model,test_data[,-31],type='class')
confusionMatrix(predicted_value,test_data$Class,dnn = c("Predictions", "Actual Values"),mode="everything")
#F1_Score(predicted_value,test_data$Class,positive = 1)
##########################################################################

# Model- 3: Decision TREE Modeling with RUS&ROS Method

#########################################################################

#Model-3.1-RUS&ROS Method

#Performance both ROS AND RUS

new<-nrow(train_data)
fraction_fraud_new<-0.50
sampling_result<-ovun.sample(Class~.,data=train_data,method = 'both',N=new,p=fraction_fraud_new,seed=2019)

sampling_credit_result<-sampling_result$data
table(sampling_credit_result$Class)
#Result     
# 0     1 
# 20132 19868 
##################################

#Model-3.2-Decision Tree with RUS&ROS Method 
decisionTree_model <- rpart(Class ~ . ,sampling_credit_result, method = 'class')
predicted_value <- predict(decisionTree_model,test_data[,-31], type = 'class')
rpart.plot(decisionTree_model,extra="auto",type=2,tweak=0.80)
summary(decisionTree_model)

#Performance Evoluation for Decision Tree for RUS&ROS 
predicted_value<-predict(decisionTree_model,test_data[,-31],type='class')
confusionMatrix(predicted_value,test_data$Class,dnn = c("Predictions", "Actual Values"))
F1_Score(predicted_value,test_data$Class,positive = NULL)

##########################################################################

# Model- 4: Decision TREE Modeling with Random Over Sampling(ROS) Method

#########################################################################
#Model 4.1- Random Over Sampling Method (ROS )
table(train_data$Class)
#Result:39882(0) 118 (1) 
no_legit<-39882
new_frac_legit<-0.50
new_n_total<-no_legit/new_frac_legit # = 227452/0.50
print(new_n_total) 
#new_n_total: 79764

oversampling_result<-ovun.sample(Class~.,data=train_data,method="over",N=new_n_total,seed=2019)
oversampled_credit<-oversampling_result$data
table(oversampled_credit$Class)
#Result
# 0     1 
# 39882 39882 
#########################
#Model 4.2 - Decision Tree with ROS Method
decisionTree_model <- rpart(Class ~ . ,oversampled_credit, method = 'class')
predicted_value <- predict(decisionTree_model,test_data[,-31], type = 'class')
rpart.plot(decisionTree_model,extra="auto",type=2,tweak=0.80)
summary(decisionTree_model)

#Performance Evoluation for Decision Tree for ROS
predicted_value<-predict(decisionTree_model,test_data[,-31],type='class')
confusionMatrix(predicted_value,test_data$Class,dnn = c("Predictions", "Actual Values"))
F1_Score(predicted_value,test_data$Class,positive = NULL)
##########################################################################
