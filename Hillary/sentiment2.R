###读取文件，
#Sys.setlocale("LC_ALL", "C") 
library(readr)
library(syuzhet)
library(RSQLite)
db <- dbConnect(dbDriver("SQLite"), "D:/Kaggle/HillaryEmail/output/database.sqlite")
#library(dplyr)
###希拉里发送邮件的情感分析
Emails <- dbGetQuery(db, "
SELECT p.Name Sender,
                        ExtractedBodyText EmailBody
                        FROM Emails e
                        INNER JOIN Persons p ON e.SenderPersonId=P.Id
                        WHERE p.Name='Hillary Clinton'
                        AND e.ExtractedBodyText != ''
                        ORDER BY RANDOM()")
###################################
#  Sentiment Analysis             #
###################################
d<-get_nrc_sentiment(Emails$EmailBody)
#d1=get_sentiment(Emails$RawText)
td<-data.frame(t(d))

td_new <- data.frame(rowSums(td[2:1882]))
#Transformation and  cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

#Vizualisation
library("ggplot2")
qplot(sentiment, data=td_new2, weight=count, geom="histogram",fill=sentiment)+ggtitle("Hillary's sending sentiment Email")
