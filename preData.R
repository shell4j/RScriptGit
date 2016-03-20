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
y=train$TARGET

###构建模型
library(xgboost)
param <- list("objective" = "binary:logistic",booster = "gbtree",
              "eval_metric" = "auc",colsample_bytree = 0.85, 
              subsample = 0.95)
param1 <- list("objective" = "binary:logistic",
               booster = "gbtree",
               "eval_metric" = "auc",
               colsample_bytree = 0.85, 
               subsample = 0.8)
train$TARGET=NULL
xgb.model=xgboost(data = as.matrix(train),
                  label=y,
                  params = param1,
                  nrounds = 300, 
                  max.depth = 12, 
                  eta = 0.015,
                  maximize = T)
test$TARGET=NULL
preds=predict(xgb.model,as.matrix(test))


submission <- data.frame(ID=test$ID,TARGET=preds)
write.csv(submission, "D:/santander/xgb_submission.csv", row.names = F)
library(dplyr)
submission[submission$TARGET>0.9,]$TARGET=0.9
submission[submission$TARGET>0.8&&submission$TARGET<0.9,]$TARGET=0.9
submission[submission$TARGET>0.6&&submission$TARGET<0.8,]$TARGET=0.8
submission[submission$TARGET>0.5&&submission$TARGET<0.6,]$TARGET=0.6
submission[submission$TARGET<0.133&&submission>0.1,]=0.1
submission[submission$TARGET<0.1&&submission>0.05,]=0.05
write.csv(submission, "D:/santander/xgb_submission1.csv", row.names = F)

