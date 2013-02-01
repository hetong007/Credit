library(MASS) #just for the first time

ridge.cv<-function(formula,set,cross=5,smp,varn)#could add other parameter to control the model input
{
  if (is.factor(set[,1]))
    set[,1]=as.numeric(set[,1])-1
  y=gety(formula,data=set)
  n=nrow(set)
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
      model=lm.ridge(formula,data=set[train,c(1,vars)])
      pred=apply(set[test,vars],2,as.numeric)%*%as.matrix(coef(model)[-1],ncol=1)+coef(model)[1]
      ans[test,j]=pred
      aucres=aucres+auc(y[test],pred)
    }
    show(j)
    aucr=c(aucr,aucres/cross)
    show(aucr[j])
  }

  return(list(AUC=aucr,Prob=ans))
}
system.time((a=ridge.cv(worse~.,train,smp=smp,varn=varn)))
ridge.pl=a$Prob
ridge.auc=a$AUC
save(ridge.pl,file="ridge.pl.rda")
save(ridge.auc,file="ridge.auc.rda")

