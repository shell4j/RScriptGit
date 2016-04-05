library(readr)
library(dplyr)
##dealing with data
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

##building model
library(xgboost)
param <- list("objective" = "binary:logistic",
              booster = "gbtree",
              "eval_metric" = "auc",
              #lambda=2,
              alpha=1,
              colsample_bytree = 0.85, 
              subsample = 0.80)
param1 <- list("objective" = "binary:logistic",
               booster = "gbtree",
               "eval_metric" = "auc",
               #lambda=2,
               colsample_bytree = 0.9, 
               subsample = 0.8)
train$TARGET=NULL
xgb.model=xgboost(data = as.matrix(train),
                  label=y,
                  params = param,
                  nrounds = 100, 
                  max.depth = 12, 
                  eta = 0.04,
                  maximize = T)
test$TARGET=NULL
preds=predict(xgb.model,as.matrix(test))
submission <- data.frame(ID=test$ID,TARGET=preds)
write.csv(submission, "D:/santander/xgb_L2_submission.csv", row.names = F)
preds[preds>0.6]=0.88
preds[preds<0]=0
preds[preds<0.133 & preds>0.1]=0.1
preds[preds<0.2 & preds>0.1]=0.1
preds[preds<0.3 & preds>0.2]=0.2
submission <- data.frame(ID=test$ID,TARGET=preds)
write.csv(submission, "D:/santander/xgb_L2_submission1.csv", row.names = F)

