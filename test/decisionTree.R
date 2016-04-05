data=read.csv("D:/Kaggle/DMinART/post_chineseAB.csv",stringsAsFactors=FALSE)
c=c(3:9,15,16,10)
data=data[,c]
#names(data)=c("国家","语言","性别","年龄",
#              "就业状况","职业内容","教育水平","收入水平","y")
for (i in ncol(data)){
  data[[i]]=as.factor(data[[i]])
}
write.csv(data,"D:/Kaggle/DMinART/decision_chineseAB.csv")


library(rpart)
fit=rpart(B1~.,data=data)
plot(fit)


data1=read.csv("D:/Kaggle/DMinART/reasonD.csv",stringsAsFactors=FALSE)
data=cbind(data,data1)