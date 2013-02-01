rpart.cv<-function(formula,set,cross=5,smp,cp)#could add other parameter to control the model input
{
	if (is.factor(set[,1]))
		set[,1]=as.numeric(set[,1])-1
	y=gety(formula,data=set)
	n=nrow(set)
	m=ceiling(n/cross)
	set.seed(1024)
	#smp=matrix(sample(1:n,n),ncol=m)
	
	ans=rep(0,n)
	aucres=0
	for (i in 1:cross)
	{
		test<-smp[i,]
		train<-setdiff(1:n,test)
		
		model=rpart(formula,data=set[train,],method="anova",cp=cp)
		pred=predict(model,newdata=set[test,])
		
		ans[test]=pred
		
		aucres=aucres+auc(y[test],pred)
	}

	aucres=aucres/cross

	return(list(AUC=aucres,Prob=ans))
}