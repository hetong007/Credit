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

#####rpart.std####
rpart.std<-function(prob){return((prob-min(prob))/(max(prob)-min(prob)))}
rpart.pl1=apply(rpart.pl,2,rpart.std)