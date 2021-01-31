#DATA Exploration

#####################
#Libraries

library(plotrix)
library(ggplot2)
library(lattice)
library(dbplyr)
library(corrplot)
##########################################################

# Import creditcart.csv data and viewed detail of dataset.
# You can download the dataset via the given link: https://www.kaggle.com/mlg-ulb/creditcardfraud
creditcard <- read.csv("C:/Users/Gulsev/Desktop/ACM514 FINAL PROJECT/creditcard.csv", header=TRUE)
View(creditcard)
###################################################################
#Glance at the structure of the dataset

str(creditcard)
summary (creditcard$Amount)
summary (creditcard$Class)
length(creditcard$Class)
#length:284807
names(creditcard)
sum(is.na(creditcard))
summary(as.factor(creditcard$Class))
#Result Summary
#0          1 
#284315    492 
###########################################
#Checks Variance of Amount and Class columns seperately
var(creditcard$Amount)
var(creditcard$Class)
################################
#Checks standart deviation of Amount and Class columns seperately
sd(creditcard$Amount)
sd(creditcard$Class)
#########################################
#CreditCCard Class defined as factor with "0" and "1" levels.
creditcard$Class<-factor(creditcard$Class,levels=c(0,1))

#Get the distribution of fraud and legit transcation in the dataset 
table(creditcard$Class)

#Get the percentage of fraud and legit transactions in the dataset
prop.table(table(creditcard$Class))


##########################################################################
#Data Visualization
#########################################################################

#Distribution of variable 'Time' by Class

creditcard %>%
  ggplot(aes(x = Time, fill = factor(Class))) + geom_histogram(bins = 100)+
  labs(x = 'Time in seconds since first transaction', y = 'No. of transactions') +
  ggtitle('Distribution of time of transaction by class') +
  facet_grid(Class ~ ., scales = 'free_y') + common_theme
##########################################################################################
#Correlation Graph of anonymised variables and 'Amount'
correlations <- cor(creditcard[,-31],method="pearson")
corrplot(correlations, number.cex = .9, method = "circle", type = "full", tl.cex=0.8,tl.col = "black")
#########################################################################################
#Distribution of variable 'Amount' by class
ggplot(creditcard, aes(x = factor(Class), y = Amount)) + geom_boxplot() + 
  labs(x = 'Class', y = 'Amount') +
  ggtitle("Distribution of transaction amount by class") + common_theme

#################################################################################################
#3D Pie Chart 
A<- c(Fraud=90,Legit=10,)
pct<-paste(round(100*prop.table(table(creditcard$Class)),2))
# calculate percentages
lbls <- paste(names(A),pct,"%")	# add percents to labels
pie3D(A,col=c("ORANGE","steelblue"),labels=lbls,
      labelcex = 1.2,
      explode=0,
      theta = 1,main=' Credit Cart Transaction Pie Chart'
)
##################################################################################################



