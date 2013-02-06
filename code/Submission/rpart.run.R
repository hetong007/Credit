
library(rpart)
rpart.run=function(formula,cplist,cpfreq,trainset,testset)
{
	if (is.factor(trainset[,1]))
		trainset[,1]=as.numeric(trainset[,1])-1
	pred=rep(0,nrow(testset))
	for (i in 1:length(cplist))
	{
		model=rpart(formula,data=trainset,method="anova",cp=cplist[i])
		pred=pred+predict(model,newdata=testset)*cpfreq[i]
		show(i); show(cplist[i]); show(cpfreq[i])
	}
	return(pred)
}

rpart.col=f3.es[[1]][f3.es[[1]]<=20]
cplist=as.numeric(names(table(rpart.col)))/1000
cpfreq=table(rpart.col)
rpart.pred=rpart.run(worse~.,cplist=cplist,cpfreq=cpfreq,
                     trainset=train,testset=test)
rpart.pred1=apply(as.matrix(rpart.pred,ncol=1),2,rpart.std)



