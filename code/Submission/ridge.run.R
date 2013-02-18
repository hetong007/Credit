library(MASS)


ridge.run=function(formula,varlist,varfreq,trainset,testset)
{
  pred=rep(0,nrow(testset))
  if(is.factor(trainset[,1]))
    trainset[,1]=as.numeric(trainset[,1])-1
  for (i in 1:ncol(varlist))
  {
    varind=which(varlist[,i]==1)+1
    model=lm.ridge(formula,data=trainset[,c(1,varind)])
    pred=pred+model.std((apply(testset[,varind],2,as.numeric)%*%as.matrix(coef(model)[-1])+coef(model)[1]))*varfreq[i]
    show(i);show(pred[1:10])
  }
  return(pred)
}

ridge.id=f3.es1[[1]][f3.es1[[1]]>120&f3.es1[[1]]<=170]-120
var=as.numeric(names(table(ridge.id)))
varlist=as.matrix(model.var[,var])
varfreq=table(ridge.id)
ridge.pred=ridge.run(worse~.,varlist=varlist,varfreq=varfreq,trainset=train,testset=test)




col=ridge.es[[1]]
rvarlist=ridge.var[,col]
train1=train; train1$worse=as.numeric(train1$worse)-1
ridge.pred=ridge.run(worse~.,varlist=rvarlist,trainset=train1,testset=test)
id=1:nrow(test)


rvarlist=ridge.var[,(sort(ini.f3.es[[1]])[3:31]-70)]
ridge.pred=ridge.run(worse~.,varlist=rvarlist,trainset=numtrain,testset=test)


pred1=ridge.pred/length(col)
r.pred=data.frame(id=id,pred=(pred1-min(pred1))/(max(pred1)-min(pred1)))
write.csv(r.pred,file="ridge.pred.csv",row.names=F,col.names=F)