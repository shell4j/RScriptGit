library(readr)
library(dplyr)
##数据预处理
train=read_csv("D:/santander/train1.csv")
test=read_csv("D:/santander/test1.csv")
train1=filter(train,TARGET>0)
train2=filter(train,TARGET<1)
s=sample_n(train2,35000)
train=rbind(train1,train1,s)
train[is.na(train)]=0
test[is.na(test)]=0
train_names <- names(train)[-1]
for (i in train_names)
{
  if (class(train[[i]]) == "integer") 
  {
    u <- unique(train[[i]])
    if (length(u) == 1) 
    {
      train[[i]] <- NULL
      test[[i]]<-NULL
    } 
  }
}
test$TARGET=NULL
train1=NULL
train2=NULL
s=NULL
gc()
library(randomForest)
library(foreach)
library(parallel)
library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)
rf.model<- foreach(ntree=rep(200, 3), 
                   .combine=combine,
                   .packages='randomForest') %dopar%
                   randomForest(TARGET~.,data=train,ntree = 400,
                        importance=TRUE)
stopCluster(cl)


preds=predict(rf.model,as.matrix(test))
submission <- data.frame(ID=test$ID,TARGET=preds)
submission[submission$TARGET>0.8,]=0.88
submission[submission$TARGET<0,]=0
write.csv(submission, "D:/santander/rf_submission.csv", row.names = F)

submission[submission$TARGET>0.6&&submission$TARGET<0.8,]=0.8
submission[submission$TARGET>0.5&&submission$TARGET<0.6,]=0.6
submission[submission$TARGET<0.133&&submission>0.1,]=0.1
submission[submission$TARGET<0.1&&submission>0.05]=0.05
#submission[,submission$TARGET<0]=0.05
write.csv(submission, "D:/santander/rf_submission1.csv", row.names = F)
