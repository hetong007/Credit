elimNA=function(set,k)
{
	ptm <- proc.time()
	set1=scale(set)
	attributes(set1)[[3]]<-NULL
	attributes(set1)[[3]]<-NULL
	rownames(set1)<-NULL
	n=nrow(set1)
	set1=set1[1:(n-1),]
	n=n-1
	show("scale complete!")
	show(proc.time() - ptm)
	ptm <- proc.time()
	m=ncol(set1)
	colind=NULL
	for (i in 1:m)
		if (sum(is.na(set1[,i]))>0)
			colind=c(colind,i)
	show("begin!")
	for (v in colind)
	{
		nind=which(is.na(set1[,v]))
		aind=(1:n)[-nind]
		aind=sample(aind,size=floor(length(aind)/5))
		for (i in nind)
		{
			rec=matrix(c(rep(10000,k),rep(0,k)),ncol=2)
			mind=k
			tmp=set1[i,]
			for (j in aind)
			{
				dst=mean((tmp-set1[j,])^2,na.rm=T)
				if (rec[mind,1]>dst)
				{
					rec[mind,]=c(dst,set[j,v])
					mind=which(rec[,1]==max(rec[,1]))[1]
				}
			}
			set[i,v]=mean(rec[,2])
			
			show(c(i,v))
			show(proc.time() - ptm)
			ptm <- proc.time()
		}
	}
	return(set)
}

filltrain=elimNA(train[,2:11],10)
save(filltrain,file="filltrain.rda")

filltest=elimNA(test[,2:11],10)
save(filltest,file="filltest.rda")

train[,2:11]=filltrain
test[,2:11]=filltest