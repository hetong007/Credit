ridge.var=matrix(rep(0,17*50),ncol=50)
ridge.pl=NULL
ridge.auc=NULL
for (i in 1:50)
{
  system.time((a=ridge.cv(worse~.,train,smp=smp)))
  ridge.pl=cbind(ridge.pl,a$Prob)
  ridge.var[a$vars,i]=1
  ridge.auc=cbind(ridge.auc,a$AUC)
  show(a$vars)
  show(a$AUC)
}