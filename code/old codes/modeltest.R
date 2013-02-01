###########################
###   Regression Tree   ###
###########################
rpart.test=rpart(worse~.,data=train)

rpart.pred=predict(rpart.test,type="prob")
pred <- prediction(rpart.pred[,2], train$worse)
performance(pred,"auc")@y.values
plot(performance(pred,"tpr","fpr"))

##########################
### LogisticRegression ###
##########################
logit.test=glm(worse~.,data=train,family=binomial)

logit.pred=predict(logit.test)
pred <- prediction(logit.pred,train$worse)
performance(pred,"auc")@y.values
plot(performance(pred,"tpr","fpr"))

###########################
###         SVM         ###
###########################
svm.test=svm(worse~.,data=train,scale=T,kernel="sigmoid",probability=T)

svm.pred=predict(svm.test)
pred <- prediction(svm.pred,train$worse)
performance(pred,"auc")@y.values
plot(performance(pred,"tpr","fpr"))

###########################
###        boost        ###
###########################
boost.test=boosting(worse~.,data=train)

boost.pred=predict(boost.test,newdata=train)
pred <- prediction(boost.pred$prob[,2],train$worse)
performance(pred,"auc")@y.values
plot(performance(pred,"tpr","fpr"))

boostind=sample(1:150000,10000)
performance(prediction(boost.pred$prob[boostind,2],train$worse[boostind]),"auc")@y.values[[1]]


###########################
###          rF         ###
###########################
library(randomForest)
set.seed(200)
rfind=sample(1:150000,10000)
system.time((rf.test=randomForest(worse~., data=train[rfind,])))
rf.pred=predict(rf.test,type="prob")

pred <- prediction(rf.pred[,2],train$worse[rfind])
performance(pred,"auc")@y.values
plot(performance(pred,"tpr","fpr"))
