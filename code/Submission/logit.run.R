
logit.run=function(formula,varlist,varfreq,trainset,testset)
{
	pred=rep(0,nrow(testset))
	for (i in 1:ncol(varlist))
	{
		varind=which(varlist[,i]==1)+1
		model=glm(formula,data=trainset[,c(1,varind)],family=binomial)
		pred=pred+1/(1+exp(-predict(model,newdata=testset[,varind])))*varfreq[i]
		show(i)
	}
	return(pred)
}
logit.id=f3.es[[1]][f3.es[[1]]>20&f3.es[[1]]<=70]-20
var=as.numeric(names(table(logit.id)))
varlist=as.matrix(model.var[,var])
varfreq=table(logit.id)
logit.pred=logit.run(worse~.,varlist=varlist,varfreq=varfreq,trainset=train,testset=test)


