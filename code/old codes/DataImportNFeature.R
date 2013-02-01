raw.train=read.csv("cs-training.csv",header=T)
raw.test=read.csv("cs-test.csv",header=T)
train=raw.train
test=raw.test

names(train)=c("id","worse","per","age","d30","ratio","income","N_open_debt","d90","N_real_estate","d60","N_dependents")
names(test)=c("id","worse","per","age","d30","ratio","income","N_open_debt","d90","N_real_estate","d60","N_dependents")

#names(train)=c("x","Pred","Card","Age","Small","DebtRatio","Income","All","Big","Mortgage","Middle","Family")
#names(test)=c("x","Pred","Card","Age","Small","DebtRatio","Income","All","Big","Mortgage","Middle","Family")

train[,1]<-NULL
test[,1]<-NULL
#test[,2]<-NULL

for (i in 2:ncol(train))
	train[,i]=as.numeric(train[,i])

train[,1]=as.factor(train[,1])
	
###############################
#####      freaturing     #####
###############################

ind96=which(train$d30==96)
ind98=which(train$d30==98)
ind999=which(train$per==0.9999999)

train$d30[ind96]=rep(NA,length(ind96))
train$d30[ind98]=rep(NA,length(ind98))
train$d60[ind96]=rep(NA,length(ind96))
train$d60[ind98]=rep(NA,length(ind98))
train$d90[ind96]=rep(NA,length(ind96))
train$d90[ind98]=rep(NA,length(ind98))

is96=rep(0,nrow(train))
is96[ind96]=1
is96=as.factor(is96)

is98=rep(0,nrow(train))
is98[ind98]=1
is98=as.factor(is98)

is999=rep(0,nrow(train))
is999[ind999]=1
is999=as.factor(is999)

train=cbind(train,is96,is98,is999)

		
rawratio=train$ratio
rawper=train$per
train$ratio[train$ratio>3.9]=3.9
bigratio=as.factor(as.numeric(train$ratio>1.9))
bigper=as.factor(as.numeric(train$per>1))
train$per[train$per>1]=1
train=cbind(train,bigratio,bigper)

rawincome=train$income
train$is0=as.factor(as.numeric(train$income==0))
train$is0[which(is.na(train$is0))]=0
train$income[which(train$income==0)]=NA
train$income=log(train$income)


###############################
#####         test        #####
###############################


for (i in 2:ncol(test))
	test[,i]=as.numeric(test[,i])

#test[,1]=as.factor(test[,1])
	

ind96=which(test$d30==96)
ind98=which(test$d30==98)
ind999=which(test$per==0.9999999)

test$d30[ind96]=rep(NA,length(ind96))
test$d30[ind98]=rep(NA,length(ind98))
test$d60[ind96]=rep(NA,length(ind96))
test$d60[ind98]=rep(NA,length(ind98))
test$d90[ind96]=rep(NA,length(ind96))
test$d90[ind98]=rep(NA,length(ind98))

is96=rep(0,nrow(test))
is96[ind96]=1
is96=as.factor(is96)

is98=rep(0,nrow(test))
is98[ind98]=1
is98=as.factor(is98)

is999=rep(0,nrow(test))
is999[ind999]=1
is999=as.factor(is999)

test=cbind(test,is96,is98,is999)

		
rawratio=test$ratio
rawper=test$per
test$ratio[test$ratio>3.9]=3.9
bigratio=as.factor(as.numeric(test$ratio>1.9))
bigper=as.factor(as.numeric(test$per>1))
test$per[test$per>1]=1
test=cbind(test,bigratio,bigper)

rawincome=test$income
test$is0=as.factor(as.numeric(test$income==0))
test$is0[which(is.na(test$is0))]=0
test$income[which(test$income==0)]=NA
test$income=log(test$income)