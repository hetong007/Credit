
library(rpart)
rpart.test=function(formula,cplist,cpfreq,trainset,testset)
{
	if (is.factor(trainset[,1]))
		trainset[,1]=as.numeric(trainset[,1])-1
	pred=NULL
	for (i in 1:length(cplist))
	{
		model=rpart(formula,data=trainset,method="anova",cp=cplist[i])
		pred=cbind(pred,predict(model,newdata=testset)*cpfreq[i])
		show(i); show(cplist[i]); show(cpfreq[i])
	}
	return(pred)
}


cplist=(1:20)/1000
cpfreq=rep(1,20)
rpart.pred=rpart.test(worse~.,cplist=cplist,cpfreq=cpfreq,
                     trainset=train,testset=test)
save(rpart.pred,file="rpart.testpred.rda")

rpart.pred1=apply(rpart.pred,2,rpart.std)

rpart.pred2=apply(rpart.pred1[,f3.es[[1]][f3.es[[1]]<=20]],1,sum)



