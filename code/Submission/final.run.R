id=1:nrow(test)

bayes.report=data.frame(id=id,pred=bayes.pred/length(bayes.es[[1]]))
write.csv(bayes.report,file="bayes.csv",row.names=F,col.names=F)


final.pred=data.frame(id=id,pred=apply(cbind(rpart.pred2,bayes.pred,ridge.pred),1,sum)/length(f3.es1[[1]]))

write.csv(final.pred,file="RpartLogitbayesRidge.std.csv",row.names=F,col.names=F)
