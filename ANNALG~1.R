#ANN Algorithm 
creditcard <- read.csv("C:/Users/Gulsev/Desktop/ACM514 FINAL PROJECT/creditcard.csv",nrows =50000  ,header=TRUE)
View(creditcard)
row(creditcard)


library(caTools)
set.seed(123)
data_sample=sample.split(creditcard$Class,SplitRatio = 0.65)

train_data=subset(creditcard,data_sample==TRUE)
test_data=subset(creditcard,data_sample==FALSE)

dim(train_data)  #40000    31
dim(test_data)   #10000    31
table(train_data$Class) 
#  0     1 
#39882   118
table(test_data$Class)
# 0    1 
#9970   30 

######################################################
#MODELING ANN PERFORMANCE TRIED WITH DIFFERENT SAMPLING METHODS
#####################################################

#MODELING_1_ANN WITH OVERSAMPLING 

#1.1-Random Over Sampling Method (ROS )
table(train_data$Class)
#  0     1 -- %80 TRAIN- %20 TEST RESULT
#39882   118
#######

#0        1  --%65 TRAIN % 35 TEST RESULT
#32404    96 
no_legit<-32404
new_frac_legit<-0.50
new_n_total<-no_legit/new_frac_legit # = 227452/0.50
print(new_n_total) 
#new_n_total: 79764 %80- %20 RESULT
#64808 - %65 -%35 RESULT
library(ROSE)
oversampling_result<-ovun.sample(Class~.,data=train_data,method="over",N=new_n_total,seed=2019)
oversampled_credit<-oversampling_result$data
table(oversampled_credit$Class)
#0      1 
#39882 39882  
#Result for %65-%35 
#0     1 
#32404 32404 

############################################
#1.2- Artificial Neural Network  with ROS 
#ANN MODELING PART WORKING HERE FOR RANDOM OVERSAMPLING MODEL

library(neuralnet)

set.seed(1)

ANN_model <- neuralnet (Class~.,oversampled_credit,hidden = c(5), act.fct = "logistic",linear.output=FALSE)

plot(ANN_model)

# Computing Predictions of the Model
predANN=compute(ANN_model,test_data[,-31])

resultANN=predANN$net.result # YSA'nin nümerik probability sonuclari

head(resultANN)

#Converting probabilities into binary classes setting threshold level 0.5
resultANN_2 <- as.vector(ifelse(predANN$net.result > 0.5, "1", "0"))
head(resultANN_2)
head(test_data$Class)

references<-as.factor(test_data$Class)

head(references)
length (references)
length(predANN_class)
predANN_class <- factor(resultANN_2, levels = c("0","1"))

levels(references)
levels(predANN_class)

predANN_class <- factor(predANN_class, levels=c("0", "1"))
table(predANN_class)
library(caret)
confusionMatrix(data = predANN_class, reference = references, dnn = c("Predictions", "Real/Actual Values"))
#PERFORMANCE EVOLUATION RESULT ANN WITH ROS

            #Real/Actual Values
#Predictions     0      1
     # 0        17434   52
     # 1          14     0
#################
# Accuracy : 0.9962          
# 95% CI : (0.9952, 0.9971)
# No Information Rate : 0.997           
# P-Value [Acc > NIR] : 0.9745          
# 
# Kappa : -0.0013
#my decision ros bad one for use with ann 
#####################################################
#MODELING-2- ANN with RANDOM UnderSampling(RUS)

#2.1-Random Under Sampling Method Apply

table(train_data$Class)
#Result:
#0     1 - %80 % 20 
#39882   118-  %80 % 20 result

#0         1  65-35 result 
#32404    96 

no_fraud<- 96 
new_frac_fraud<-0.50
new_n_total<-no_fraud/new_frac_fraud

undersampling_result<-ovun.sample(Class~.,data=train_data,method="under",N=new_n_total,seed=123)

undersampled_credit<-undersampling_result$data
table(undersampled_credit$Class)
#Result 80/20 
#0   1 
#118 118  
#RESULT 65/35 
#96(0)/96(1)

#2.2-APPLY ANN ALGORITHM FOR RUS METHOD


library(neuralnet)

set.seed(1)

ANN_model <- neuralnet (Class~.,undersampled_credit,hidden = c(10), act.fct = "logistic",linear.output=FALSE)

plot(ANN_model)

# Computing Predictions of the Model
predANN=compute(ANN_model,test_data[,-31])

resultANN=predANN$net.result # YSA'nin nümerik probability sonuclari

head(resultANN)

#Converting probabilities into binary classes setting threshold level 0.5
resultANN_2 <- as.vector(ifelse(predANN$net.result > 0.5, "1", "0"))
head(resultANN_2)

head(test_data$Class)

references<-as.factor(test_data$Class)
head(references)

predANN_class <- factor(resultANN_2, levels = c("0","1"))
levels(references)
levels(predANN_class)


predANN_class <- factor(predANN_class, levels=c("0", "1"))
table(predANN_class)

library(caret)
confusionMatrix(data = predANN_class, reference = references, dnn = c("Predictions", "Real/Actual Values"))

#               Real/Actual Values
# Predictions     0      1
# 0             17252    19
# 1               196    33

# Accuracy : 0.9877         
# 95% CI : (0.986, 0.9893)
# No Information Rate : 0.997          
# P-Value [Acc > NIR] : 1              
# 
# Kappa : 0.2312  
##################
#Modeling 3- ANN WITH SMOTE METHOD 
#3.1 Smote method implementation
#Smote legnthleri duzgun dagitmiyor o yuzden performans duzgun olculemiyor.Kullanma smote


#####################

#Modeling 4.1- ANN WITH Performance both ROS AND RUS
library(ROSE)
table(train_data$Class)
# 0        1 
# 32404    96 

new<-nrow(train_data)
fraction_fraud_new<-0.50
sampling_result<-ovun.sample(Class~.,data=train_data,method = 'both',N=new,p=fraction_fraud_new,seed=2019)

sampling_credit_result<-sampling_result$data
table(sampling_credit_result$Class)
#Result     
#  0     1 
# 16272 16228 
#4.2- ANN MODELING FOR ROS&RUS METHOD

library(neuralnet)

set.seed(1)

ANN_model <- neuralnet (Class~.,sampling_credit_result,hidden = c(5), act.fct = "logistic",linear.output=FALSE)

plot(ANN_model)

# Computing Predictions of the Model
predANN=compute(ANN_model,test_data[,-31])


resultANN=predANN$net.result # YSA'nin nümerik probability sonuclari

head(resultANN)

#Converting probabilities into binary classes setting threshold level 0.5
resultANN_2 <- as.vector(ifelse(predANN$net.result > 0.5, "1", "0"))
head(resultANN_2)
head(test_data$Class)

references<-as.factor(test_data$Class)

head(references)
length (references)
length(predANN_class)
predANN_class <- factor(resultANN_2, levels = c("0","1"))
predANN_class <- factor(predANN_class, levels=c("0", "1"))
table(predANN_class)
levels(references)
levels(predANN_class)
library(caret)
confusionMatrix(data = predANN_class, reference = references, dnn = c("Predictions", "Real/Actual Values"))

