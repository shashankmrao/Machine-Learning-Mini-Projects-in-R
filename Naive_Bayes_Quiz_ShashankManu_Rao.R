#install.packages('e1071')
library(e1071)

#Loading dataset
credit<-read.csv("Credit_Dataset.csv")
#Creating categorical variable called PROFITABLE
credit$PROFITABLE<-ifelse(credit$PROFIT>=0,1,0)
#Creating factor variables
credit$CHK_ACCTF<-as.factor(credit$CHK_ACCT)
credit$SAV_ACCTF<-as.factor(credit$SAV_ACCT)
credit$HISTORYF<-as.factor(credit$HISTORY)
credit$JOBF<-as.factor(credit$JOB)
credit$TYPEF<-as.factor(credit$TYPE)
#Setting seed
set.seed(12345)
#Splitting between test and rest of the data
test_ins<-sample(nrow(credit),0.3*nrow(credit))
credit_test<-credit[test_ins,]
credit_rest<-credit[-test_ins,]

#Splitting rest of the data into validation and training data
valid_ins<-sample(nrow(credit_rest),0.25*nrow(credit_rest))
credit_valid<-credit_rest[valid_ins,]
credit_train<-credit_rest[-valid_ins,]
attach(credit)

#Function to calculate accuracy
class_performance <- function(confusion_matrix){
  
  TP <- confusion_matrix[2,2]
  TN <- confusion_matrix[1,1]
  FP <- confusion_matrix[1,2]
  FN <- confusion_matrix[2,1]
  acc <- (TP+TN)/(TP+TN+FP+FN)
  return(acc)
}
#Function to calculate confusion matrix
confusion_matrix <- function(preds, actuals, cutoff){
  
  classifications <- ifelse(preds>cutoff,1,0)
  
  
  confusion_matrix <- table(actuals,classifications)
}
#Training a NaiveBayes model
modelBayes<-naiveBayes(PROFITABLE~AGE+DURATION+RENT+TELEPHONE+FOREIGN+CHK_ACCTF+SAV_ACCTF+HISTORYF+JOBF+TYPEF,data=credit_train,type="class")

bayes_preds <- predict(modelBayes,newdata=credit_valid,type='raw')
predbayes <- prediction(bayes_preds[,2],credit_valid$PROFITABLE)
Bayes_matrix_valid <- confusion_matrix(bayes_preds[,2], credit_valid$PROFITABLE,.5)
Bayes_pred_valid_acc <- class_performance(Bayes_matrix_valid)
Bayes_pred_valid_acc
#1)Accuracy on validation data is 0.72 given cutoff =0.5


acc.perf1 = performance(predbayes, measure = "acc")

best1 = which.max(slot(acc.perf1,"y.values")[[1]])
max.acc1 = slot(acc.perf1,"y.values")[[1]][best1]
max.cutoff1 = slot(acc.perf1,"x.values")[[1]][best1]
print(c(ACC= max.acc1, cutoff = max.cutoff1))
#Optimal cutoff=0.5288004 for validation data for highest accuracy=0.7314286


#Using Naive Bayes model to calculate accuracy on test data for cutoff=0.5
Bayes_pred_test <- predict(modelBayes,newdata=credit_test,type='raw')

Bayes_matrix_test2 <- confusion_matrix(Bayes_pred_test[,2], credit_test$PROFITABLE,.5)

Bayes_pred_test_acc <- class_performance(Bayes_matrix_test2)
Bayes_pred_test_acc

#Using Naive Bayes model to calculate accuracy on test data for optimal cutoff=0.5288004
Bayes_pred_test <- predict(modelBayes,newdata=credit_test,type='raw')

Bayes_matrix_test2 <- confusion_matrix(Bayes_pred_test[,2], credit_test$PROFITABLE,.5288004)

Bayes_pred_test_acc <- class_performance(Bayes_matrix_test2)
Bayes_pred_test_acc

#1)Accuracy on validation data is 0.72 given cutoff =0.5

#2)Logistic regression model accuracy with 0.6619577 optimal cutoff: 0.683333
#Tree accuracy: 0.71
#kNN accuracy: 0.73
#Naive Bayes Accuracy with 0.5 cutoff=0.68
#Naive Bayes Accuracy with optimal cutoff of 0.5288004:0.67
#Thus, Naive Bayes model gives us the least accuracy for the credit dataset

#3)Naive Bayes model has the least accuracy and thus cannot be used as a classifier for this dataset going forward.

