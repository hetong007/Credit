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



####################################################################

####################################################################

smp=sm(worse=train$worse,seed=1024,cross=5)

rpart.cv<-function(formula,set,cross=5,smp,cp)#could add other parameter to control the model input
{
	if (is.factor(set[,1]))
		set[,1]=as.numeric(set[,1])-1
	y=gety(formula,data=set)
	n=nrow(set)
	m=ceiling(n/cross)
	set.seed(1024)
	#smp=matrix(sample(1:n,n),ncol=m)
	
	ans=rep(0,n)
	aucres=0
	for (i in 1:cross)
	{
		test<-smp[i,]
		train<-setdiff(1:n,test)
		
		model=rpart(formula,data=set[train,],method="anova",cp=cp)
		pred=predict(model,newdata=set[test,])
		
		ans[test]=pred
		
		aucres=aucres+auc(y[test],pred)
	}

	aucres=aucres/cross

	return(list(AUC=aucres,Prob=ans))
}


#########################
####        logit
#########################


logit.cv<-function(formula,set,cross=5,smp,k=10)#could add other parameter to control the model input
{
	y=gety(formula,data=set)
	n=nrow(set)
	m=ceiling(n/cross)
	#set.seed(1024)
	#smp=matrix(sample(1:n,n),ncol=m)
	ans=rep(0,n)
	aucres=0
	varn=sample(4:16,1)
	vars=sample(2:17,varn)

	for (i in 1:cross)
	{
		test<-smp[i,]
		train<-setdiff(1:n,test)

		model=glm(formula,data=set[train,c(1,vars)],family=binomial)
		pred=predict(model,newdata=set[test,])
		ans[test]=pred
		aucres=aucres+auc(y[test],pred)
	}

	aucres=aucres/cross

	return(list(AUC=aucres,Var=vars,Prob=ans))
}
system.time((a=logit.cv(worse~.,train,smp=smp,k=10)))
#The real test shows that 15W data need about 140sec, maybe a better computer could reduce to 120sec
#AUC is around 0.78


ridge.cv<-function(formula,set,cross=5,smp)#could add other parameter to control the model input
{
  if (is.factor(set[,1]))
    set[,1]=as.numeric(set[,1])-1
  y=gety(formula,data=set)
  n=nrow(set)
  ans=rep(0,n)
  aucres=NULL
  for (i in 1:cross)
  {
    test<-smp[i,]
    train<-setdiff(1:n,test)
      model=lm.ridge(formula,data=set[train,])
      pred=apply(set[test,-1],2,as.numeric)%*%as.matrix(coef(model)[-1],ncol=1)+coef(model)[1]
      ans[test]=pred
      aucres=c(aucres,auc(y[test],ans[test]))
  }
  
  AUC.MEAN=mean(aucres)
  AUC.SD=sd(aucres)
  
  return(list(AUC.MEAN=AUC.MEAN,AUC.SD=AUC.SD,aucres=aucres,Prob=ans))
}
system.time((a=ridge.cv(worse~.,train,smp=smp)))



sufvar=function(n=16,m=7)
{
	numans=0
	for (i in m:n) 
		numans=numans+choose(n,i)
	return(combn(x=n,m=m))
}

chooseVar=sufvar(16,7)


smp=sm(worse=train$worse,seed=1024,cross=5)

rpart.pl=mat.or.vec(150000,15)
auc.pl=rep(0,15)

rpart.pl=cbind(rpart.pl,mat.or.vec(150000,5))
auc.pl=c(auc.pl,rep(0,5))
for (i in 1:5)
{
	cp=i/1000
	res=rpart.cv(worse~.,train,cp=cp,smp=smp)
	auc.pl[i]=res$AUC
	rpart.pl[,i]=res$Prob
	show(i)
}

ind.pl=c(1:6,8:10,14,15)




logit.pl=mat.or.vec(150000,50)
logit.auc=rep(0,50)
nvars=ncol(chooseVar)
for (i in 1:50)
{
	