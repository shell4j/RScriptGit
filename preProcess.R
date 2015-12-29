##读取SQLite文件 并探索数据， 注意字符编码
#！！！！Sys.setlocale("LC_ALL", "C") 
library(readr)
library(syuzhet)
library(RSQLite)
db <- dbConnect(dbDriver("SQLite"), "D:/Kaggle/HillaryEmail/output/database.sqlite")
######表的初探  四张表的大小#####
library(dplyr)
tables <- dbGetQuery(db, "SELECT Name FROM sqlite_master WHERE type='table'")
colnames(tables) <- c("Name")
tables <- tables %>%
  rowwise() %>%
  mutate(RowCount=dbGetQuery(db, paste0("SELECT COUNT(Id) RowCount FROM ", Name))$RowCount[1])
print(tables)

######希拉里发送的邮件分布
commonSenders <- dbGetQuery(db, "
                            SELECT p.Name, COUNT(p.Name) NumEmailsSent
                            FROM Emails e
                            INNER JOIN Persons p ON e.SenderPersonId=p.Id
                            GROUP BY p.Name
                            ORDER BY COUNT(p.Name) DESC
                            LIMIT 20")

library(ggplot2)
ggplot(commonSenders, aes(x=reorder(Name, NumEmailsSent), y=NumEmailsSent)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=16) +
  xlab("") +
  ylab("Number of Emails Sent") + 
  theme(plot.title=element_text(size=14))

####希拉里接受邮件的人物分布
commonRecipients <- dbGetQuery(db, "
SELECT p.Name, COUNT(p.Name) NumEmailsReceived
FROM Emails e
INNER JOIN EmailReceivers r ON r.EmailId=e.Id
INNER JOIN Persons p ON r.PersonId=p.Id
GROUP BY p.Name
ORDER BY COUNT(p.Name) DESC
LIMIT 15")

library(ggplot2)
ggplot(commonRecipients, aes(x=reorder(Name, NumEmailsReceived), y=NumEmailsReceived)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=16) +
  xlab("") + 
  ylab("Number of Emails Received") + 
  theme(plot.title=element_text(size=14))


#####找出希拉里写的邮件的内容
emailsFromHillary <- dbGetQuery(db, "
SELECT p.Name Sender,
                                ExtractedBodyText EmailBody
                                FROM Emails e
                                INNER JOIN Persons p ON e.SenderPersonId=P.Id
                                WHERE p.Name='Hillary Clinton'
                                AND e.ExtractedBodyText != ''
                                ORDER BY RANDOM()")

####将希拉里写的email进行词云分析
library(tm)
library(wordcloud)
makeWordCloud <- function(documents) {
  corpus = Corpus(VectorSource(tolower(documents)))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  frequencies = DocumentTermMatrix(corpus)
  word_frequencies = as.data.frame(as.matrix(frequencies))
  
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[400]],
            colors=brewer.pal(8, "Dark2"),
            random.color=TRUE)  
}
makeWordCloud(emailsFromHillary[["EmailBody"]])