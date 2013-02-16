
ensemble=function(pool,ans,smp,maxn,replacement=T,cross=5,maxauc=0,cind=NULL,avail=NULL,cumcsn=NULL)#pool must be validation pool
{
	#cind=NULL#the list of selected ensemble
	
	nc=ncol(pool)
	nr=nrow(pool)
	if (is.null(avail))
		avail=rep(TRUE,nc)#the logical vector indicating available models in the pool
	if (is.null(cumcsn))
		cumcsn=rep(0,nr)#the cumulative sum pool of chosen models
	#maxauc=0#the max auc 
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

rpart.es=ensemble(rpart.pl[,ind.pl],ans=train$worse,smp=smp,maxn=10,replacement=T)#AUC is 0.8460484

logit.es=ensemble(logit.pl,ans=train$worse,smp=smp,maxn=10,replacement=T)#AUC is 0.8568039

bayes.es=ensemble(bayes.pl,ans=train$worse,smp=smp,maxn=30,replacement=T)#AUC is 0.8568039




rpart.logit.es=ensemble(cbind(rpart.pl,logit.pl),ans=train$worse,smp=smp,maxn=20,replacement=T)#AUC is 0.8581255

ridge.es=ensemble(ridge.pl,ans=train$worse,smp=smp,maxn=30,replacement=T)#AUC is 0.8577097


f3.es1=ensemble(cbind(rpart.pl,logit.pl,bayes.pl,ridge.pl),ans=train$worse,smp=smp,maxn=30,replacement=T)#AUC is 0.8590509


ini.f3.es=iniensemble(cbind(rpart.pl,logit.pl,ridge.pl),k=15,ans=train$worse,smp=smp,maxn=50,replacement=T)#AUC is 0.8582039
