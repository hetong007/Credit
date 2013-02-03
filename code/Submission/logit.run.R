logit.run=function(formula,varlist,trainset,testset)
{
	pred=rep(0,nrow(testset))
	for (i in 1:ncol(varlist))
	{
		varind=which(varlist[,i]==1)
		model=glm(formula,data=trainset[,c(1,varind)],family=binomial)
		pred=pred+predict(model,newdata=testset[,varind])
		show(i)
	}
	return(pred)
}

varlist=logit.var[,c(40,50)]
logit.pred=logit.run(worse~.,varlist=varlist,trainset=train,testset=test)