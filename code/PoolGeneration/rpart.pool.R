rpart.pl=mat.or.vec(150000,20)
auc.pl=rep(0,20)

for (i in 1:20)
{
	cp=i/1000
	res=rpart.cv(worse~.,train,cp=cp,smp=smp)
	rpart.auc[i]=res$AUC
	rpart.pl[,i]=res$Prob
	show(rpart.auc[i])
}