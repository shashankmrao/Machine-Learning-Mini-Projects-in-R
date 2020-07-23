

library(tree)
library(class)
library(ROCR)
#1)a)Loading dataset
credit<-read.csv("Credit_Dataset.csv")
#1)b)Creating categorical variable called PROFITABLE
credit$PROFITABLE<-ifelse(credit$PROFIT>=0,1,0)
#1)c)Creating factor variables
credit$CHK_ACCTF<-as.factor(credit$CHK_ACCT)
credit$SAV_ACCTF<-as.factor(credit$SAV_ACCT)
credit$HISTORYF<-as.factor(credit$HISTORY)
credit$JOBF<-as.factor(credit$JOB)
credit$TYPEF<-as.factor(credit$TYPE)
#1)d)Setting seed
set.seed(12345)
#Splitting between test and rest of the data
test_ins<-sample(nrow(credit),0.3*nrow(credit))
credit_test<-credit[test_ins,]
credit_rest<-credit[-test_ins,]

#1)e)Splitting rest of the data into validation and training data
valid_ins<-sample(nrow(credit_rest),0.25*nrow(credit_rest))
credit_valid<-credit_rest[valid_ins,]
credit_train<-credit_rest[-valid_ins,]
attach(credit)

#2)Training a logistic regression model
modelLog<-glm(PROFITABLE~AGE+DURATION+RENT+TELEPHONE+FOREIGN+CHK_ACCTF+SAV_ACCTF+HISTORYF+JOBF+TYPEF,data=credit_train,family="binomial")
summary(modelLog)

log_preds <- predict(modelLog,newdata=credit_valid,type="response")
predt <- prediction(log_preds,credit_valid$PROFITABLE)



tpr.perf1 = performance(predt, measure = "tpr")
tnr.perf1 = performance(predt, measure = "tnr")
acc.perf1 = performance(predt, measure = "acc")

#2)a)Plotting accuracy, sensitivity, specificity at all cutoff values for the validation data 
plot(tpr.perf1,ylim=c(0,1),col="red")
plot(tnr.perf1, add=T,col="blue")
plot(acc.perf1, add=T, col="green")

#Calculating the cutoff value for the highest accuracy
best1 = which.max(slot(acc.perf1,"y.values")[[1]])
max.acc1 = slot(acc.perf1,"y.values")[[1]][best1]
max.cutoff1 = slot(acc.perf1,"x.values")[[1]][best1]
print(c(ACC= max.acc1, cutoff = max.cutoff1))

#2)b)Calculating ROC curve values for both validation and training data
roc.valid1 = performance(predt, measure = "tpr", x.measure = "fpr")
log_preds_train <- predict(modelLog,newdata=credit_train,type="response")
predt_train <- prediction(log_preds_train,credit_train$PROFITABLE)
roc.train1= performance(predt_train, measure = "tpr", x.measure = "fpr")

#Plotting ROC curves for both validation and training data
plot(roc.valid1,col="red")
par(new=TRUE)
plot(roc.train1,col="blue")
abline(a=0,b=1,lty=3)

#2)c)Plotting lift curve for validation data
lift.valid1 = performance(predt, measure = "lift", x.measure = "rpp")
plot(lift.valid1)

#Finding highest lift value that can be achieved
best2 = which.max(slot(lift.valid1,"y.values")[[1]])
max.lift1 = slot(lift.valid1,"y.values")[[1]][best2]
max.lift1

#3)Classification tree algorithm to predict PROFITABLE
credit.tree=tree(PROFITABLE~AGE+DURATION+RENT+TELEPHONE+FOREIGN+CHK_ACCTF+SAV_ACCTF+HISTORYF+JOBF+TYPEF,credit_train)
summary(credit.tree)
plot(credit.tree)
text(credit.tree,pretty=1)

#Creating pruned trees with different number of terminal nodes
credit.pruned2=prune.tree(credit.tree,best=2)
credit.pruned4=prune.tree(credit.tree,best=4)
credit.pruned6=prune.tree(credit.tree,best=6)
credit.pruned8=prune.tree(credit.tree,best=8)
credit.pruned10=prune.tree(credit.tree,best=10)

#Function to calculate accuracy with a tree model and a cutoff value
predict_and_classify <- function(treename, pred_data, actuals, cutoff){
  probs <- predict(treename, newdata = pred_data)
  classifications <- ifelse(probs>cutoff,1,0)
  accuracy <- sum(ifelse(classifications==actuals,1,0))/length(actuals)
  return(accuracy)
}
#3)a)Calculating accuracies for validation data with different pruned trees
pruned_va_2 <- predict_and_classify(credit.pruned2, credit_valid, credit_valid$PROFITABLE, .5)
pruned_va_4 <- predict_and_classify(credit.pruned4, credit_valid, credit_valid$PROFITABLE, .5)
pruned_va_6 <- predict_and_classify(credit.pruned6, credit_valid, credit_valid$PROFITABLE, .5)
pruned_va_8 <- predict_and_classify(credit.pruned8, credit_valid, credit_valid$PROFITABLE, .5)
pruned_va_10 <- predict_and_classify(credit.pruned10, credit_valid, credit_valid$PROFITABLE, .5)
full_va_acc <- predict_and_classify(credit.tree, credit_valid, credit_valid$PROFITABLE, .5)

va_acc<-c(pruned_va_2,pruned_va_4,pruned_va_6,pruned_va_8,pruned_va_10,full_va_acc)

#Calculating accuracies for training data with different pruned trees
pruned_tr_2 <- predict_and_classify(credit.pruned2, credit_train, credit_train$PROFITABLE, .5)
pruned_tr_4 <- predict_and_classify(credit.pruned4, credit_train, credit_train$PROFITABLE, .5)
pruned_tr_6 <- predict_and_classify(credit.pruned6, credit_train, credit_train$PROFITABLE, .5)
pruned_tr_8 <- predict_and_classify(credit.pruned8, credit_train, credit_train$PROFITABLE, .5)
pruned_tr_10 <- predict_and_classify(credit.pruned10, credit_train, credit_train$PROFITABLE, .5)
full_tr_acc <- predict_and_classify(credit.tree, credit_train, credit_train$PROFITABLE, .5)

tr_acc<-c(pruned_tr_2,pruned_tr_4,pruned_tr_6,pruned_tr_8,pruned_tr_10,full_tr_acc)

#Plotting accuracies of training and validation data with different pruned trees as well as unpruned tree
plot(c(2,4,6,8,10,11),va_acc,ylim=c(0,1),type="b",col="red",xlab="Number of terminal nodes",ylab="Accuracy")
par(new=TRUE)
plot(c(2,4,6,8,10,11),tr_acc,ylim=c(0,1),type="b",col="blue",xlab="Number of terminal nodes",ylab="Accuracy")

#3)b)Plotting the best pruned tree with the highest accuracy
plot(credit.pruned6)
text(credit.pruned6,pretty=1)

#3)c)Calculatng number of decision nodes of the best pruned tree
terminal_nodes6 = length(unique(credit.pruned6$where))
total_nodes6 = nrow(credit.pruned6$frame)
decision_nodes6 =  total_nodes6-terminal_nodes6
decision_nodes6

#Calculatng number of decision nodes of the unpruned tree
terminal_nodes_full = length(unique(credit.tree$where))
total_nodes_full = nrow(credit.tree$frame)
decision_nodes_full =  total_nodes_full-terminal_nodes_full
decision_nodes_full

#4)kNN algorithm for classification on the training data 

train.profitable=as.numeric(credit_train$PROFITABLE)
valid.profitable=as.numeric(credit_valid$PROFITABLE)
test.profitable=as.numeric(credit_test$PROFITABLE)

#Function to normalize training data values
normalise <- function (my_column)
{
  my_range = max(my_column)-min(my_column)
  my_column = (my_column-min(my_column))/my_range
  
  return(my_column)
  
}

#Function to normalize validation and test data values
normalize <- function(newdataf, dataf){
  normalizeddataf <- newdataf 
  for (n in names(newdataf)){
    normalizeddataf[,n] <-  
      (newdataf[,n] - min(dataf[,n])) /  (max(dataf[,n]) -  min(dataf[,n]))
  } 
  return(normalizeddataf)
}

#Normalizing training data variables
AGE_normal = normalise(credit_train$AGE)
CHK_normal = normalise(credit_train$CHK_ACCT)
SAV_normal = normalise(credit_train$SAV_ACCT)
DURATION_normal = normalise(credit_train$DURATION)
HISTORY_normal = normalise(credit_train$HISTORY)
JOB_normal = normalise(credit_train$JOB)
RENT_normal = normalise(credit_train$RENT)
TELEPHONE_normal = normalise(credit_train$TELEPHONE)
FOREIGN_normal = normalise(credit_train$FOREIGN)
TYPE_normal = normalise(credit_train$TYPE)

train.X1=data.frame(AGE_normal,CHK_normal,SAV_normal,DURATION_normal,HISTORY_normal,JOB_normal,RENT_normal,TELEPHONE_normal,FOREIGN_normal,TYPE_normal)

#Normalizing validation and test data variables
valid.X1=normalize(credit_valid[,c(2,3,4,6,7,10,12,17,18,20)],credit_train[,c(2,3,4,6,7,10,12,17,18,20)])
test.X1=normalize(credit_test[,c(2,3,4,6,7,10,12,17,18,20)],credit_train[,c(2,3,4,6,7,10,12,17,18,20)])


#4)a)
#Function to classify probabilities to 0 and 1
knn.classification <- function(winning_class, probs, cutoff){
  #prob that y=1 is the probability of 1 if 1 was the winning class, 0 otherwise
  prob_of_1 <- ifelse(winning_class==1,probs,1-probs)
  classification <- ifelse(prob_of_1>cutoff,1,0)
  return(classification)
}

#Function to calculate accuracy from confusion matrix
class_performance <- function(confusion_matrix){
  
  TP <- confusion_matrix[2,2]
  TN <- confusion_matrix[1,1]
  FP <- confusion_matrix[1,2]
  FN <- confusion_matrix[2,1]
  acc <- (TP+TN)/(TP+TN+FP+FN)
  tpr <- TP/(TP+FN)
  tnr <- TN/(TN+FP)
  return(acc)
}

#Function to calculate accuracy for different k values
knn_accuracy <- function(training,newdata,pred_train_var,pred_newdata_var,i){
  knn.predtestn=knn(training,newdata,pred_train_var,prob=T,k=i)
  prob.testn <- attr(knn.predtestn, "prob")
  class.testn <- knn.classification(knn.predtestn,prob.testn,.5)
  acc.testn=class_performance(table(pred_newdata_var,class.testn))
  return(acc.testn) 
}

#Calculating accuracy for different k values for validation data
acc.va1=knn_accuracy(train.X1,valid.X1,train.profitable,valid.profitable,1)
acc.va3=knn_accuracy(train.X1,valid.X1,train.profitable,valid.profitable,3)
acc.va5=knn_accuracy(train.X1,valid.X1,train.profitable,valid.profitable,5)
acc.va7=knn_accuracy(train.X1,valid.X1,train.profitable,valid.profitable,7)
acc.va11=knn_accuracy(train.X1,valid.X1,train.profitable,valid.profitable,11)
acc.va15=knn_accuracy(train.X1,valid.X1,train.profitable,valid.profitable,15)
acc.va21=knn_accuracy(train.X1,valid.X1,train.profitable,valid.profitable,21)
acc.va25=knn_accuracy(train.X1,valid.X1,train.profitable,valid.profitable,25)
acc.va31=knn_accuracy(train.X1,valid.X1,train.profitable,valid.profitable,31)
acc.va35=knn_accuracy(train.X1,valid.X1,train.profitable,valid.profitable,35)

acc.va=c(acc.va1,acc.va3,acc.va5,acc.va7,acc.va11,acc.va15,acc.va21,acc.va25,acc.va31,acc.va35)

#Calculating accuracy for different k values for training data
acc.tr1=knn_accuracy(train.X1,train.X1,train.profitable,train.profitable,1)
acc.tr3=knn_accuracy(train.X1,train.X1,train.profitable,train.profitable,3)
acc.tr5=knn_accuracy(train.X1,train.X1,train.profitable,train.profitable,5)
acc.tr7=knn_accuracy(train.X1,train.X1,train.profitable,train.profitable,7)
acc.tr11=knn_accuracy(train.X1,train.X1,train.profitable,train.profitable,11)
acc.tr15=knn_accuracy(train.X1,train.X1,train.profitable,train.profitable,15)
acc.tr21=knn_accuracy(train.X1,train.X1,train.profitable,train.profitable,21)
acc.tr25=knn_accuracy(train.X1,train.X1,train.profitable,train.profitable,25)
acc.tr31=knn_accuracy(train.X1,train.X1,train.profitable,train.profitable,31)
acc.tr35=knn_accuracy(train.X1,train.X1,train.profitable,train.profitable,35)

acc.tr=c(acc.tr1,acc.tr3,acc.tr5,acc.tr7,acc.tr11,acc.tr15,acc.tr21,acc.tr25,acc.tr31,acc.tr35)

#Plotting accuracies for different k values for training and validation data
plot(c(1,3,5,7,11,15,21,25,31,35),acc.va,ylim=c(0,1),type="b",col="red",xlab="k value",ylab="Accuracy")
par(new=TRUE)
plot(c(1,3,5,7,11,15,21,25,31,35),acc.tr,ylim=c(0,1),type="b",col="blue",xlab="k value",ylab="Accuracy")


#5)
#Using logistic regression model to calculate accuracy on test data
log_preds_test <- predict(modelLog,newdata=credit_test,type="response")

#Function to calculate confusion matrix
confusion_matrix <- function(preds, actuals, cutoff){
  
  classifications <- ifelse(preds>cutoff,1,0)
  
  
  confusion_matrix <- table(actuals,classifications)
}

#Calculating Test accuracy for cutoff 0.5
log_matrix_test <- confusion_matrix(log_preds_test, credit_test$PROFITABLE,.5)

log_test_acc <- class_performance(log_matrix_test)
log_test_acc

#Calculating Test accuracy for optimal cutoff 0.6619577
log_matrix_test2 <- confusion_matrix(log_preds_test, credit_test$PROFITABLE,.6619577)

log_test_acc2 <- class_performance(log_matrix_test2)
log_test_acc2

#Calculating test accuracy using the classification optimal pruned tree model
pruned_test_acc_6 <- predict_and_classify(credit.pruned6, credit_test, credit_test$PROFITABLE, .5)
pruned_test_acc_6

#Calculating test accuracy with kNN algorithm with k=3 (optimal value)
acc.test3=knn_accuracy(train.X1,test.X1,train.profitable,test.profitable,3)
acc.test3
