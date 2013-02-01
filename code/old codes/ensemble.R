auc<-function(outcome,prob){
	if (is.factor(outcome))
		outcome=as.numeric(outcome)-1
	N=length(prob)
	N_pos=sum(outcome)
	outcome=outcome[order(-prob)]
	above=(1:N)-cumsum(outcome)
	return(1-sum(above*outcome)/(N_pos*(N-N_pos)))
}


#0.08 sec for each run on average
###### stratified random sampling ########
sm<-function(worse,seed,cross=5)
{
  zz0=which(worse==0)
  nr0=length(zz0)
  zz1=which(worse==1)
  nr1=length(zz1)
  set.seed(seed)
  aa0=sample(rep(1:cross,ceiling(nr0/cross))[1:nr0],nr0)
  aa1=sample(rep(1:cross,ceiling(nr1/cross))[1:nr1],nr1)
  dd=list()
  for (i in 1:cross) dd[[i]]=union(zz1[aa1==i],zz0[aa0==i])
  return(matrix(unlist(dd),nrow=cross))
}

smp=sm(worse=train$worse,seed=1024,cross=5)

###### pool ensemble: to be improved ############
ensemble=function(pool,ans,smp,maxn,replacement=T,cross=5,seed=1024)#pool must be validation pool
{
	cind=NULL#the list of selected ensemble
	
	nc=ncol(pool)
	nr=nrow(pool)
	
	avail=rep(TRUE,nc)#the logical vector indicating available models in the pool
	
	cumcsn=rep(0,nr)#the cumulative sum pool of chosen models
	maxauc=0#the max auc 
	#set.seed(seed)
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
					topflag=FALSE#not peak yet
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
testesb=function(nc,nr,maxn,replacement=T)
{
	p=NULL
	for (i in 1:nc){
		p=cbind(p,runif(nr))
	}
	ssmmpp=sm(worse=train$worse[1:nr],seed=1024,cross=5)
	ans=as.numeric(train$worse[sample(1:150000,nr)])-1
  return(ensemble(p,ans,smp=ssmmpp,maxn=maxn,replacement=replacement))
}
system.time((a=testesb(nc=10,nr=150000,maxn=10,replacement=T)))

rpart.es=ensemble(rpart.pl[,ind.pl],ans=train$worse,smp=smp,maxn=10,replacement=T)#AUC is 0.8460484

logit.es=ensemble(logit.pl,ans=train$worse,smp=smp,maxn=10,replacement=T)#AUC is 0.8564055

rpart.logit.es=ensemble(cbind(rpart.pl,logit.pl),ans=train$worse,smp=smp,maxn=20,replacement=T)#AUC is 0.857526

ridge.es=ensemble(ridge.pl,ans=train$worse,smp=smp,maxn=10,replacement=T)#AUC is 0.8577097


f3.es=ensemble(cbind(rpart.pl,logit.pl,ridge.pl),ans=train$worse,smp=smp,maxn=20,replacement=T)#AUC is 0.857526
