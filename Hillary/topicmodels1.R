###topicmodels 发送给希拉里邮件的主题分析
library(readr)
library(RSQLite)
library(topicmodels)
library(tm)
db <- dbConnect(dbDriver("SQLite"), "D:/Kaggle/HillaryEmail/output/database.sqlite")
emailsToHillary <- dbGetQuery(db, "
SELECT p.Name Recipient,
       ExtractedBodyText EmailBody
FROM Emails e
INNER JOIN EmailReceivers r ON r.EmailId=e.Id
INNER JOIN Persons p ON r.PersonId=P.Id
WHERE p.Name='Hillary Clinton'
  AND e.ExtractedBodyText != ''
ORDER BY RANDOM()")
vcorp = VCorpus(VectorSource(emailsToHillary$EmailBody))

docterm_mat = DocumentTermMatrix(vcorp)
drop_inds = which(rowSums(as.matrix(docterm_mat))==0)
docterm_mat = docterm_mat[-drop_inds,]

num_topics = 25 # Max that memory limit can handle
fitted_lda = LDA(docterm_mat,k=num_topics)

num_terms = 20
aa=terms(fitted_lda,num_terms)
write_csv(as.data.frame(aa),"D:/Kaggle/HillaryEmail/topicOutput1.csv")