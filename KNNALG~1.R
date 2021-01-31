#Libraries
library(caTools)
library(MLmetrics)
library (caret)   
library(lattice)
library(ggplot2)
library(ROSE)
library(smotefamily)
library(pROC)
###########################
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

# Model- 1:kNN Regression Modeling with Random Under- Sampling(RUS)Method

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
###############Model 1.2- Modeling kNN algorithm  with RUS 

set.seed(250)
ctrl_knn <- trainControl(method = "repeatedcv", repeats = 10, number = 3)
model_knn <- train(Class~., data = undersampled_credit, method = "knn",preProcess = c("center","scale"), trControl = ctrl_knn, tuneLength = 40)
pred_knn<- predict(model_knn, test_data[,-31])

length(test_data$Class)
length(pred_knn)
levels(pred_knn)
levels(test_data$Class)
conf_table_knn <- confusionMatrix(test_data$Class, pred_knn, mode = "everything", positive = "1")
conf_table_knn
#########

plot(model_knn)
#####################################
#Roc curve of knn
roc.curve(test_data$Class, pred_knn, plotit = TRUE)

################################
#2.1 -SMOTE to Balance The Dataset
library(smotefamily)
table(train_data$Class)
#Result:39882(0) 118(1) 
n0<-39882 #= legit cases
n1<-118   #=fraud cases
r0<-0.6
ntimes<-((1-r0)/r0)*(n0/n1)-1
smote_output=SMOTE(X=train_data[,-c(1,31)],target=train_data$Class,K=3,dup_size=ntimes)

credit_smote_result<-smote_output$data
colnames(credit_smote_result)[30]<-"Class"
prop.table(table(credit_smote_result$Class))
#Smote result : 0.6005365(0) 0.3994635(1) 


###############
#2.2-Modeling knn with SMOTE METHOD

set.seed(250)
ctrl_knn <- trainControl(method = "repeatedcv", repeats = 10, number = 3)
model_knn <- train(Class~., data = credit_smote_result, method = "knn",preProcess = c("center","scale"), trControl = ctrl_knn, tuneLength = 40)
pred_knn<- predict(model_knn, test_data[,-31])

length(test_data$Class)
length(pred_knn)
levels(pred_knn)
levels(test_data$Class)
conf_table_knn <- confusionMatrix(test_data$Class, pred_knn, mode = "everything", positive = "1")
conf_table_knn
#########

plot(model_knn)
#####################################
#ROC Curve- F1 Score-auc of KNN
roc.curve(test_data$Class, pred_knn, plotit = TRUE)
F1_Score(pred_knn,test_data$Class,positive = NULL)
plot(margin(pred_knn,test_data$Class))