#Data Preparation
##################

library(caret)
library(ggplot2)
library(lattice)
library(ROSE)
library(plotrix)
library(caTools)
library(smotefamily)
#Model Prediction is not started yet!!!

creditcard$Class<-factor(creditcard$Class,levels=c(0,1))
predictions<-rep.int(0,nrow(creditcard))
predictions<-factor(predictions,levels = c(0,1))
confusionMatrix(data=predictions,reference = creditcard$Class,mode = "everything")
#Scatter plot  drawing for unbalance data
ggplot(data = creditcard,aes(x=V1,y=V2,col=as.factor(Class)))+
  geom_point(position = position_jitter(width = 0.2)) +
  theme_bw()+
  scale_color_manual(values = c('dodgerblue2','red'))
##############################
#creditcard.csv file will use just 50000 rows instead of 284775 for handle performance problem.

creditcard <- read.csv("C:/Users/Gulsev/Desktop/ACM514 FINAL PROJECT/creditcard.csv",nrows =50000  ,header=TRUE)
View(creditcard)
row(creditcard)
##############
#Creating Test &Training Datasets 


set.seed(123)
data_sample=sample.split(creditcard$Class,SplitRatio = 0.8)
train_data=subset(creditcard,data_sample==TRUE)
test_data=subset(creditcard,data_sample==FALSE)

dim(train_data)
dim(test_data)
table(train_data$Class)
table(test_data$Class)
##################################################
#SAMPLING METHODS IMPLEMENTATIONS
##################################################

#Random Over Sampling Method (ROS )
table(train_data$Class) 
#  0     1 
# 39882   118 
no_legit<-39882
new_frac_legit<-0.50
new_n_total<-no_legit/new_frac_legit # = 39882/0.50
print(new_n_total) 
#new_n_total: 79764

oversampling_result<-ovun.sample(Class~.,data=train_data,method="over",N=new_n_total,seed=2019)
oversampled_credit<-oversampling_result$data
table(oversampled_credit$Class)
# 0     1 
# 39882 39882 
##########
#ROS Scatter PLOT DRAW

ggplot(data = oversampled_credit,aes(x=V1,y=V2,col=as.factor(Class)))+
  geom_point(position = position_jitter(width = 0.2)) +
  theme_bw()+
  scale_color_manual(values = c('dodgerblue2','red'))
#################
#3D Pie Chart Result After Over Sampling(ROS)

b<-c(Legit=90,Fraud=10)
x<-paste(round(100*prop.table(table(oversampled_credit$Class)), 2))
y <- paste(names(b),x,"%")	
pie3D(b,col=c("gray","steelblue"),
      labels=y,
      labelcex = 1.8,
      explode=0,
      theta = 1,
      main=" After Over-Sampling (ROS)Usage Pie chart of credit card transaction  ")

##############################
# Random Under- Sampling(RUS)Method
table(train_data$Class)
#Result:39882(0)  118 (1) 
no_fraud<-118
new_frac_fraud<-0.50
new_n_total<-no_fraud/new_frac_fraud

undersampling_result<-ovun.sample(Class~.,data=train_data,method="under",N=new_n_total,seed=123)

undersampled_credit<-undersampling_result$data
table(undersampled_credit$Class)
#Result
#0   1 
#118 118 
############################3
#Scatter Plot for RUS
#It takes some time to show plot. 
ggplot(data = undersampled_credit,aes(x=V1,y=V2,col=as.factor(Class)))+
  geom_point(position = position_jitter(width = 0.2)) +
  theme_bw()+
  scale_color_manual(values = c('dodgerblue2','red'))
################################################
#Performance both ROS AND RUS
table(train_data$Class)
#Result:39882(0)  118 (1) 
new<-nrow(train_data)
#new shows 40000
fraction_fraud_new<-0.50
sampling_result<-ovun.sample(Class~.,data=train_data,method = 'both',N=new,p=fraction_fraud_new,seed=2019)

sampling_credit_result<-sampling_result$data
table(sampling_credit_result$Class)
#Result     
#0      1 
#114038 113808 
###############################3
#Scatter Plot for ROS&RUS Result

ggplot(data = sampling_credit_result,aes(x=V1,y=V2,col=as.factor(Class)))+
  geom_point(position = position_jitter(width = 0.2)) +
  theme_bw()+
  scale_color_manual(values = c('dodgerblue2','red'))
####################################################
#3D Pie Chart Result After Over Sampling(ROS& RUS)
b<-c(Legit=90,Fraud=10)
x<-paste(round(100*prop.table(table(sampling_credit_result$Class)), 2))
y <- paste(names(b),x,"%")	
pie3D(b,col=c("lightgreen","orange"),
      labels=y,
      labelcex = 1.8,
      explode=0,
      theta = 1,
      main=" ROS & RUS Together Usage Pie chart of credit card transaction  ")

###########################################################

#SMOTE to Balance The Dataset
table(train_data$Class)
#Result:39882(0)  118 (1) 
n0<-39882 #= legit cases
n1<-118    #=fraud cases
r0<-0.6
ntimes<-((1-r0)/r0)*(n0/n1)-1
smote_output=SMOTE(X=train_data[,-c(1,31)],target=train_data$Class,K=3,dup_size=ntimes)

credit_smote_result<-smote_output$data
colnames(credit_smote_result)[30]<-"Class"
prop.table(table(credit_smote_result$Class))
#Smote result : 0.6003432(0)  0.3996568(1) 

#Scatter Smote Method
library(ggplot2)
library(lattice)
ggplot(data = credit_smote_result,aes(x=V1,y=V2,col=as.factor(Class)))+
  geom_point(position = position_jitter(width = 0.2)) +
  theme_bw()+
  scale_color_manual(values = c('dodgerblue2','red'))
#So far  dataset is prepared.  Ready to fit the dataset to a model!


