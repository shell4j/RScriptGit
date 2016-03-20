library(readr)
library(syuzhet)
library(RSQLite)
db <- dbConnect(dbDriver("SQLite"), "D:/Kaggle/HillaryEmail/output/database.sqlite")
#library(dplyr)
###选出邮件的主要内容
Emails <- data.frame(dbGetQuery(db,"SELECT * FROM Emails"))
###################################
#  Sentiment Analysis             #
###################################
d<-get_nrc_sentiment(Emails$RawText)
afinn_vector <- get_sentiment(Emails$RawText, method="afinn")
#d1=get_sentiment(Emails$RawText)
td<-data.frame(t(d))

td_new <- data.frame(rowSums(td[2:7945]))
#Transformation and  cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

#Vizualisation
library("ggplot2")
qplot(sentiment, data=td_new2, weight=count, geom="histogram",fill=sentiment)+ggtitle("sentiment Email")