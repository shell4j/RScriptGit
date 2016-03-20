library(readr) # CSV file I/O, e.g. the read_csv function
library(xgboost)
# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
# Reading the data
dat_train=read_csv("D:/santander/train.csv")
dat_test=read_csv("D:/santander/test.csv")
# Mergin the test and train data
dat_test$TARGET <- NA
all_dat <- rbind(dat_train, dat_test)

# Removing the constant variables
train_names <- names(dat_train)[-1]
for (i in train_names)
{
  if (class(all_dat[[i]]) == "integer") 
  {
    u <- unique(all_dat[[i]])
    if (length(u) == 1) 
    {
      all_dat[[i]] <- NULL
    } 
  }
}
train_names <- names(all_dat)[-1]
fac <- data.frame(fac = integer())    

for(i in 1:length(train_names))
{
  if(i != length(train_names))
  {
    for (k in (i+1):length(train_names)) 
    {
      if(identical(all_dat[,i], all_dat[,k]) == TRUE) 
      {
        fac <- rbind(fac, data.frame(fac = k))
      }
    }
  }
}
same <- unique(fac$fac)
all_dat <- all_dat[,-same]

# Splitting the data for model
train <- all_dat[1:nrow(dat_train), ]
test <- all_dat[-(1:nrow(dat_train)), ]
y=as.numeric(dat_train$TARGET)
train[is.na(train)]=0
test[is.na(test)]=0
pca=princomp(train[,-c(train$ID,train$TARGET)],cor=TRUE)
train1=predict(pca,train[,-c(train$ID,train$TARGET)])
test=predict(pca,test[,-c(test$ID,test$TARGET)])
param1 <- list("objective" = "binary:logistic",
               booster = "gbtree",
               "eval_metric" = "auc",
               colsample_bytree = 0.85, 
               subsample = 0.75)
xgb.model=xgboost(data = as.matrix(train[,-train$ID]),
                  label=y,
                  params = param1,
                  nrounds = 80, 
                  max.depth = 10, 
                  eta = 0.02,
                  maximize = T)
preds=predict(xgb.model,test)
submission <- data.frame(ID=test$ID, TARGET=preds)
write.csv(submission, "D:/santander/submission.csv", row.names = F)
