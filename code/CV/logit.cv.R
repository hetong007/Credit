logit.cv<-function(formula,set,cross=5,smp,varn)#could add other parameter to control the model input
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
      
      model=glm(formula,data=set[train,c(1,vars)],family=binomial)
      pred=1/(exp(-predict(model,newdata=set[test,]))+1)
      ans[test,j]=pred
      aucres=aucres+auc(y[test],pred)
    }
    show(j)
    show(aucres/cross)
    aucr=c(aucr,aucres/cross)
  }
	return(list(AUC=aucr,Prob=ans))
}
system.time((a=logit.cv(worse~.,train,smp=smp,varn=varn)))
logit.pl=a$Prob
logit.auc=a$AUC
save(logit.pl,file="logit.pl.rda")
save(logit.auc,file="logit.auc.rda")