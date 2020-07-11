#1) Data Preparation
#a)Reading the dataset
car <- read.csv("car_data.csv")
#b)Setting seed
set.seed(71923)
#c) Random partitioning into training and test datasets
train_insts1 = sample(nrow(car), .7*nrow(car))
cars_train <- car[train_insts1,]
cars_test <- car[-train_insts1,]

detach(cars_train)
attach(cars_train)
#2)a)Boxplots
options(scipen=999)
boxplot(VehOdo~IsBadBuy)
boxplot(VehicleAge~IsBadBuy)
#b)table of Make and their number of BadBuys
tab1<-table(Make,IsBadBuy)
tab1

#3)a)Linear model
model1A=lm(IsBadBuy~Auction+VehicleAge+Make+Color+WheelType+VehOdo+Size+MMRAcquisitionAuctionAveragePrice+MMRAcquisitionRetailAveragePrice,data=cars_train)
summary(model1A)

lin_train_preds <- predict(model1A,newdata=cars_train)
lin_test_preds <- predict(model1A,newdata=cars_test)

#RMSE and average error functions
rmse<-function(err)
{
  sqrt(mean(err^2))
}

avgerr<-function(err)
{
  mean(err)
}

err_train=IsBadBuy-lin_train_preds

err_test=cars_test$IsBadBuy-lin_test_preds

#3)b)RMSE and average error calculation
rmse_train=rmse(err_train)
avgerr_train=avgerr(err_train)

rmse_test=rmse(err_test)
avgerr_test=avgerr(err_test)

#3c)Confusion matrix and performance metrics functions
confusion_matrix <- function(preds, actuals, cutoff){
  
  classifications <- ifelse(preds>cutoff,1,0)
  
  
  confusion_matrix <- table(actuals,classifications)
}

class_performance <- function(confusion_matrix){
  
  TP <- confusion_matrix[2,2]
  TN <- confusion_matrix[1,1]
  FP <- confusion_matrix[1,2]
  FN <- confusion_matrix[2,1]
  
  ##accuracy = total number of correct classifications/total number of classifications
  acc <- (TP+TN)/(TP+TN+FP+FN)
  
  ##TPR = Percent of actual positives identified as such (sensitivity)
  tpr <- TP/(TP+FN)
  
  ##TNR = Percent of actual negatives identified as such (specificity)
  tnr <- TN/(TN+FP)
  
  
  fpr=1-tnr
  fnr=1-tpr
  ##return the list of metrics you want
  return(c(acc, tpr, tnr))
}

lin_matrix <- confusion_matrix(lin_test_preds, cars_test$IsBadBuy,.5)
lin_matrix

#3)d)To calculate accuracy on test data
lin_metrics <- class_performance(lin_matrix)
lin_metrics

#4)a)Logistic regression model
model_logA=glm(IsBadBuy~Auction+VehicleAge+Make+Color+WheelType+VehOdo+Size+MMRAcquisitionAuctionAveragePrice+MMRAcquisitionRetailAveragePrice,data=cars_train,family="binomial")
summary(model_logA)

#test predictions by log model
log_test_preds <- predict(model_logA,newdata=cars_test,type="response")

#4)c)confusion matrix for test data
log_matrix <- confusion_matrix(log_test_preds, cars_test$IsBadBuy,.5)
log_matrix
log_metrics <- class_performance(log_matrix)
log_metrics

#4)d)test_instance=c(MANHEIM,1,NISSAN,RED,NULL,10000,COMPACT,8000,10000)
test_instance <- read.csv("car_test.csv")
pred_i <- predict(model_logA,newdata=test_instance,type="response")
pred_i
