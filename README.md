#Fraud Detection Regression Analysis with R

Financial fraud is an ever-growing metus in the financial industry. Credit card frauds are easy and friendly targets.
Data mining had played an important role in the detection of credit card fraud in online transactions. Credit card fraud detection, which is a data mining problem, becomes challenging due to two major reasons - first, the profiles of normal and fraudulent behaviors change constantly, and secondly, credit card fraud data sets are highly skewed. 
I worked on the famous Kaggle Dataset Credit Card Fraud Detection, which can be found via the given link: https://www.kaggle.com/mlg-ulb/creditcardfraud
You can reach model results and regression results via paper which put in the repository.
The performance of fraud detection in credit card transactions is greatly affected by the sampling approach on the dataset, selection of variables, and detection technique(s) used. This document investigates the performance of ANN, Decision Tree, k-nearest neighbor, and Random Forest on highly skewed credit card fraud data.  The sampling techniques of under-sampling and oversampling smote and undersampling & oversampling together usage is carried out on the skewed data. The four techniques are applied to raw and preprocessed data. The work is implemented in R. The performance of the techniques is evaluated based on the confusion matrix with accuracy, sensitivity, specificity, and  AUC, F1 Score.
