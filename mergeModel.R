library(readr)
library(dplyr)
rf=read_csv("D:/santander/rf_submission.csv")
xgb=read_csv("D:/santander/xgb_submission.csv")
r=0.2
ans=r*rf$TARGET+(1-r)*xgb$TARGET
submission <- data.frame(ID=test$ID, TARGET=ans)
write.csv(submission, "D:/santander/merge_submission.csv", row.names = F)
