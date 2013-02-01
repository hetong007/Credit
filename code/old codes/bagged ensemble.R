


bagensemble<-function(pool,ans,times,p=0.5,maxn,replacement=T,cross=5,seed=1024)
{
  nc=ncol(pool)
  nmodel=floor(nc*p)
  ModelID=NULL
  ModelNum=NULL
  finauc=NULL
  smp=sm(worse=ans,seed=seed,cross=cross)
  for (i in 1:times)
  {
	set.seed(i)
    id=sample(1:nc,nmodel)
    re=ensemble(pool[,id],ans,maxn=maxn,replacement=replacement,cross=cross,seed=seed)
    ModelID=c(ModelID,re$ModelID)
	ModelNum=c(ModelNum,rep(length(re$ModelID),length(re$ModelID)))
  }
  pred=apply(pool[,ModelID],1,mean)
  for (ii in 1:cross)
  {
    finauc=c(finauc,auc(ans[smp[ii,]],pred[smp[ii,]]))
  }
  return(list(MID=ModelID,MNum=ModelNum,Mean.AUC=mean(finauc),SD.AUC=sd(finauc)))
}

#ensemble=function(pool,ans,maxn,replacement=T,cross=5,seed=1024)
testesb=function(nc,nr,maxn,times,p,replacement=T)
{
  p=NULL
  for (i in 1:nc){
    p=cbind(p,runif(nr))
  }
  ans=as.numeric(train$worse[sample(1:150000,nr)])-1
  return(bagensemble(p,ans,times=times,p=p,maxn=maxn,replacement=replacement))
}
system.time((a=testesb(nc=80,nr=100000,maxn=10,times=10,p=0.5,replacement=T)))