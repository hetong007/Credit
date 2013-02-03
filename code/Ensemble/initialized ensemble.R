
#######initialize ##########
init<-function(pool,ans,k,nc,cross,smp)
{
  auclist=NULL
  for (i in 1:nc)
  {
      tmpprob=pool[,i]#the average of models in chosen and the tested one
      tmpauc=NULL
      
      for (ii in 1:cross)
        tmpauc=c(tmpauc,auc(ans[smp[ii,]],tmpprob[smp[ii,]]))#the tested auc
      tmpauc=mean(tmpauc)
      auclist=c(auclist,tmpauc)
  }
  
  auclist=rbind((1:nc),auclist)
  auclist=auclist[,order(auclist[2,],decreasing=T)]
  rec=auclist[1,1:k]#the index of chosen models
  
  tmpprob=apply(pool[,rec],1,mean)#the average of models in chosen and the tested one
  tmpauc=NULL

  for (ii in 1:cross)
	tmpauc=c(tmpauc,auc(ans[smp[ii,]],tmpprob[smp[ii,]]))#the tested auc
  auc=mean(tmpauc)
  
  return(list(rec=rec,auc=auc))
}



####### pool ensemble: to be improved ############
iniensemble=function(pool,ans,smp,maxn,k,replacement=T,cross=5)#pool must be validation pool
{
  nc=ncol(pool)
  nr=nrow(pool)
  #smp=sm(worse=ans,seed=seed,cross=cross)
  
  ini=init(pool=pool,ans=ans,k=k,nc=nc,cross=cross,smp=smp)#the list of selected ensemble
  cind=ini[[1]]
  maxauc=ini[[2]]#the max auc
  avail=rep(TRUE,nc)#the logical vector indicating available models in the pool
  
  cumcsn=apply(pool[,cind],1,mean)#the cumulative sum pool of chosen models
  
  
  #smp=matrix(sample(1:nr,nr),ncol=ceiling(nr/cross))
  return(ensemble(pool,ans,smp,maxn,replacement=T,cross=5,maxauc=maxauc,cind=cind,avail=avail,cumcsn=cumcsn))
}
########## test ensemble ################
testesb=function(nc,nr,maxn,k,replacement=T)
{
  p=NULL
  for (i in 1:nc){
    p=cbind(p,runif(nr))
  }
  ans=as.numeric(train$worse[sample(1:150000,nr)])-1
  return(iniensemble(p,ans,maxn=maxn,k=k,replacement=replacement))
}
system.time((a=testesb(nc=80,nr=1000,maxn=10,k=5,replacement=T)))


ini.f3.es=iniensemble(cbind(rpart.pl,logit.pl,ridge.pl),k=15,ans=train$worse,smp=smp,maxn=50,replacement=T)#AUC is 0.8582039