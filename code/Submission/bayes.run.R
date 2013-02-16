

bayes.run=function(formula,varlist,varfreq,trainset,testset)
{
  pred=rep(0,nrow(testset))
  for (i in 1:ncol(varlist))
  {
    varind=which(varlist[,i]==1)+1
    model=NaiveBayes(formula,data=trainset[,c(1,varind)],fL=1)
    pred=pred+predict(model,newdata=testset)[[2]][,2]*varfreq[i]
    show(i)
  }
  return(pred)
}

varfreq=table(bayes.es[[1]])
varlist=as.matrix(model.var)[,as.numeric(names(varfreq))]
bayes.pred=bayes.run(worse~.,varlist=varlist,varfreq=varfreq,trainset=train,testset=test)

varfreq=table(f3.es[[1]][f3.es[[1]]>70&f3.es[[1]]<=120]-70)
varlist=model.var[,as.numeric(names(varfreq))]
bayes.pred=bayes.run(worse~.,varlist=varlist,varfreq=varfreq,trainset=train,testset=test)

