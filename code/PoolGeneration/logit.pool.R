logit.var=matrix(rep(0,17*50),ncol=50)
logit.pl=NULL
logit.auc=NULL
for (i in 1:50)
{
  system.time((a=logit.cv(worse~.,train,smp=smp)))
  logit.pl=cbind(logit.pl,a$Prob)
  logit.var[a$vars,i]=1
  logit.auc=cbind(logit.auc,a$AUC)
  show(a$vars)
  show(a$AUC)
}