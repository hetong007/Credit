gety=function(formula,data)#get response from a formula, code copied from "lm"
{
	mf <- match.call(expand.dots = FALSE)
	m <- match(c("formula","data"), names(mf), 0L)
	mf <- mf[c(1L, m)]
	mf$drop.unused.levels <- TRUE
	mf[[1L]] <- as.name("model.frame")
	mf <- eval(mf, parent.frame())
	response<-model.response(mf)
	return(unname(response))
}

################
#  The Outline #
################
#This is just a structure and not syntax-correct
gcv<-function(formula,set,cross=5,smp)#could add other parameter to control the model input
{
	y=gety(formula,data=set)
	n=length(y)
	m=ceiling(n/cross)
	set.seed(1024)
	smp=matrix(sample(1:n,n),ncol=m)
	
	aucres=0
	
	for (i in 1:cross)
	{
		test<-smp[i,]
		train<-setdiff(1:n,test)
		
		model=function(formula,data=set[train,])
		pred=predict(model,newdata=set[test,],type="prob")
		
		ans[test]=pred
		
		aucres=aucres+auc(y[test],pred)
		}
	}
	
	aucres=aucres/cross

	return(list(AUC=aucres,Prob=ans)
}

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


####choosen number#####
matvar=function(n=16,m=7)
{
	ans=0
	probs=NULL
	for (i in m:n) 
	{
		ans=ans+choose(n,i)
		probs=c(probs,rep(1/((n-m+1)*choose(n,i)),choose(n,i)))
	}
	vars=mat.or.vec(n,ans)
	k=1
	for (i in m:n)
	{
		chsn=choose(n,i)
		cmbn=combn(n,i)
		for (j in 1:chsn)
		{
			vars[cmbn[,j],k]=1
			k=k+1
		}
	}
	return(list(Prob=probs,Mat=vars))
}
	
choosevar=matvar(16,7)
varn=sample(1:length(choosevar[[1]]),50,prob=choosevar[[1]])
model.var=choosevar[[2]][,varn]
save(varn,file="varn.rda")
save(model.var,file="model.var.rda")

#vars=sample(1:length(choosevar[[1]]),size=50,prob=choosevar[[1]])