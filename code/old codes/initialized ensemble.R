
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
  auclist=auclist[,order(auclist[1,],decreasing=T)]
  rec=auclist[1,1:k]#the index of chosen models
  
  tmpprob=apply(pool[,rec],1,mean)#the average of models in chosen and the tested one
  tmpauc=NULL

  for (ii in 1:cross)
	tmpauc=c(tmpauc,auc(ans[smp[ii,]],tmpprob[smp[ii,]]))#the tested auc
  auc=mean(tmpauc)
  
  return(list(rec=rec,auc=auc)
}



####### pool ensemble: to be improved ############
iniensemble=function(pool,ans,smp,maxn,k,replacement=T,cross=5,seed=1024)#pool must be validation pool
{
  nc=ncol(pool)
  nr=nrow(pool)
  #smp=sm(worse=ans,seed=seed,cross=cross)
  
  ini=init(pool=pool,ans=ans,k=k,nc=nc,cross=cross,smp=smp)#the list of selected ensemble
  cind=ini[[1]]
  maxauc=ini[[2]]#the max auc
  avail=rep(TRUE,nc)#the logical vector indicating available models in the pool
  
  cumcsn=rep(0,nr)#the cumulative sum pool of chosen models
  
  
  #smp=matrix(sample(1:nr,nr),ncol=ceiling(nr/cross))
  for (tt in 1:maxn)
  {
    maxi=0#the index of selected model
    topflag=TRUE#flag of peak
    ncind=length(cind)#number of chosen models
    for (i in 1:nc)
    {
      if (avail[i])
      {
        tmpprob=(pool[,i]+cumcsn)/(ncind+1)#the average of models in chosen and the tested one
        tmpauc=NULL
        
        for (ii in 1:cross)
          tmpauc=c(tmpauc,auc(ans[smp[ii,]],tmpprob[smp[ii,]]))#the tested auc
        tmpauc=mean(tmpauc)
        if (tmpauc>maxauc)
        {
          topflag=FALSE   #not peak yet
          maxauc=tmpauc
          maxi=i
        }
      }
    }
    if (!topflag)
    {
      cind=c(cind,maxi)#add maxi into the list
      cumcsn=pool[,maxi]+cumcsn#add new cumcsn
      avail[maxi]=FALSE#not available
    }
    else
    {
      tmpcsn=cumcsn/ncind
      tmpauc=NULL
      for (i in 1:cross)
        tmpauc=c(tmpauc,auc(ans[smp[i,]],tmpcsn[smp[i,]]))#the tested auc
      if (sum(as.numeric(avail))==nc)
        return(list(ModelID=cind,AUC=tmpauc,Mean.AUC=mean(tmpauc),SD.AUC=sd(tmpauc)))
      if (replacement)
        avail=rep(TRUE,nc)#begin replacement
      else
        return(list(ModelID=cind,AUC=tmpauc,Mean.AUC=mean(tmpauc),SD.AUC=sd(tmpauc)))
    }
  }
  tmpcsn=cumcsn/ncind
  tmpauc=NULL
  for (i in 1:cross)
    tmpauc=c(tmpauc,auc(ans[smp[i,]],tmpcsn[smp[i,]]))#the tested auc
  #set.seed(proc.time())
  return(list(ModelID=cind,AUC=tmpauc,Mean.AUC=mean(tmpauc),SD.AUC=sd(tmpauc)))
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


