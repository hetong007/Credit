rpart.run=function(formula,cplist,trainset,testset)
{
	if (is.factor(trainset[,1]))
		trainset[,1]=as.numeric(trainset[,1])-1
	pred=rep(0,nrow(testset))
	for (cp in cplist)
	{
		model=rpart(formula,data=trainset,method="anova",cp=cp)
		pred=pred+predict(model,newdata=testset)
		show(cp)
	}
	return(pred)
}

cplist=c(1,1,2,3,4,5,6,7,10,11,12,13)/1000
rpart.pred=rpart.run(worse~.,cplist=cplist,trainset=train,testset=test)