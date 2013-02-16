library(e1071)

library(klaR)
bayes.cv<-function(formula,set,cross=5,smp,varn)#could add other parameter to control the model input
{
  y=gety(formula,data=set)
  n=nrow(set)
  m=ceiling(n/cross)
  ans=matrix(rep(0,n*length(varn)),nrow=n)
  aucr=NULL
  for (j in 1:length(varn))
  {
    vars=which(choosevar[[2]][,varn[j]]==1)+1
    aucres=0
    for (i in 1:cross)
    {
      test<-smp[i,]
      train<-setdiff(1:n,test)
      
      model=NaiveBayes(formula,data=set[train,c(1,vars)],fL=1)
      pred=predict(model,newdata=set[test,])[[2]][,2]
      ans[test,j]=pred
      aucres=aucres+auc(y[test],pred)
    }
    show(j)
    show(aucres/cross)
    aucr=c(aucr,aucres/cross)
  }
  return(list(AUC=aucr,Prob=ans))
}
system.time((a=bayes.cv(worse~.,train,smp=smp,varn=varn)))

bayes.pl=a$Prob
bayes.auc=a$AUC
save(bayes.pl,file="bayes.pl.rda")
save(bayes.auc,file="bayes.auc.rda")


bayes.cv<-function(formula,set,cross=5,smp)#could add other parameter to control the model input
{
  y=gety(formula,data=set)
  n=nrow(set)
  ans=matrix(rep(0,n),nrow=n)
  aucres=0
  for (i in 1:cross)
  {
    test<-smp[i,]
    train<-setdiff(1:n,test)
    
    model=NaiveBayes(formula,data=set[train,],fL=1)
    pred=predict(model,newdata=set[test,])[[2]][,2]
    ans[test]=pred
    aucres=aucres+auc(y[test],pred)
    show(auc(y[test],pred))
  }
  
  return(list(AUC=aucres/cross,Prob=ans))
}

train1=train
train1[,2:11]=predict(princomp(apply(train[,2:11],2,as.numeric)))
train2=train1[,c(1:8,12:17)]
system.time((a=bayes.cv(worse~.,train1,smp=smp)))




naive=naiveBayes(worse~., data=train[1:100000,])
pred <- predict(naive, train[100001:150000,],type="raw")
auc(train$worse[100001:150000],pred[,2])






