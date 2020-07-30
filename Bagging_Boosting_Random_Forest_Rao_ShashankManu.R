library(randomForest)
library(gbm)
library(ISLR)


setwd('/Users/Shashank/Downloads')


credit<-read.csv('Credit_Dataset.csv')
set.seed(12345)


credit$PROFITABLE <- ifelse(credit$PROFIT>0,1,0)
credit$PROFITABLE <- as.factor(credit$PROFITABLE)

#Remove profit column
profitcol <- which(colnames(credit) == "PROFIT")
credit <- credit[,-profitcol]

#Splitting test, valid and training dataset
test<- sample(nrow(credit), .3*nrow(credit))
credit_test <- credit[test,]
test_size <- length(test)

credit_rest <- credit[-test,]
valid <- sample(nrow(credit_rest), 0.25*nrow(credit_rest))
credit_valid <- credit_rest[valid,]
valid_size <- nrow(credit_valid)
credit_train <- credit_rest[-valid,]

#bagging is just a random forest where m=p
#set mtry = 21 (the number of features in the dataset)
bag.mod <- randomForest(PROFITABLE~.,data=credit_train,mtry=21,importance=TRUE)
bag_preds <- predict(bag.mod,newdata=credit_valid)
bag_acc <- sum(ifelse(bag_preds==credit_valid$PROFITABLE,1,0))/valid_size
bag_acc

#a)Validation data accuracy for bagging= 0.7142857

##random forest
#set mtry ~= sqrt(21) = 5
#set 1000 trees (this is something you can tune)
rf.mod <- randomForest(PROFITABLE~.,data=credit_train,mtry=5,ntree=1000,importance=TRUE)
rf_preds <- predict(rf.mod,newdata=credit_valid)
rf_acc <- sum(ifelse(rf_preds==credit_valid$PROFITABLE,1,0))/valid_size

rf_acc 

#b)Validation data accuracy for random forest= 0.7371429


#Boosting
boost.valid <- credit_valid
boost.valid$PROFITABLE <- ifelse(credit_valid$PROFITABLE == "1", 1, 0)

boost.train <- credit_train
boost.train$PROFITABLE <- ifelse(credit_train$PROFITABLE == "1", 1, 0)

#interaction.depth refers to the maximum depth of tree allowed
boost.mod <- gbm(PROFITABLE~.,data=boost.train,distribution="bernoulli",n.trees=1000,interaction.depth=4)
boost_preds <- predict(boost.mod,newdata=boost.valid,type='response',n.trees=1000)

#classify with a cutoff and compute accuracy
boost_class <- ifelse(boost_preds>.5,1,0)
boost_acc <- sum(ifelse(boost_class==boost.valid$PROFITABLE,1,0))/valid_size
boost_acc

#c)Validation data accuracy for boosting= 0.6914286

#On test data

#Bagging
test.bag.preds <- predict(bag.mod,newdata=credit_test)
test.bag.acc <- sum(ifelse(test.bag.preds==credit_test$PROFITABLE,1,0))/test_size

test.bag.acc
#Test data accuracy for bagging = 0.76


# Random forest 
test.rf.preds <- predict(rf.mod,newdata=credit_test)
test.rf.acc<- sum(ifelse(test.rf.preds==credit_test$PROFITABLE,1,0))/test_size

test.rf.acc
#Test data accuracy for random forest = 0.7666667

# Boosting
boost.test <- credit_test
boost.test$PROFITABLE <- ifelse(credit_test$PROFITABLE == "1", 1, 0)

test.boost.preds <- predict(boost.mod,newdata=boost.test,type='response',n.trees=1000)

test.boost.class <- ifelse(test.boost.preds>.5,1,0)
test.boost.acc <- sum(ifelse(test.boost.class==boost.test$PROFITABLE,1,0))/test_size
test.boost.acc 
#Test data accuracy for boosting = 0.75

#d)Test data accuracy for Bagging, Random Forest and Boosting models are 
# 0.76, 0.7666667, 0.75 respectively.
#With accuracy as metric, I would choose Random Forest model.

#Yes, we were able to slightly improve the accuracy as compared to Lasso, 
#Ridge logistic regressions models or any other past classifiers.





