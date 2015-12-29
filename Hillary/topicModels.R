##topicModels 希拉里发送邮件的topic Model分析
library(readr)
library(RSQLite)
library(topicmodels)
library(tm)
db <- dbConnect(dbDriver("SQLite"), "D:/Kaggle/HillaryEmail/output/database.sqlite")
emailsFromHillary <- dbGetQuery(db, "
SELECT p.Name Sender,
                                ExtractedBodyText EmailBody
                                FROM Emails e
                                INNER JOIN Persons p ON e.SenderPersonId=P.Id
                                WHERE p.Name='Hillary Clinton'
                                AND e.ExtractedBodyText != ''
                                ORDER BY RANDOM()")
vcorp = VCorpus(VectorSource(emailsFromHillary$EmailBody))

docterm_mat = DocumentTermMatrix(vcorp)
drop_inds = which(rowSums(as.matrix(docterm_mat))==0)
docterm_mat = docterm_mat[-drop_inds,]

num_topics = 25 # Max that memory limit can handle
fitted_lda = LDA(docterm_mat,k=num_topics)

num_terms = 20
terms(fitted_lda,num_terms)
aa=terms(fitted_lda,num_terms)
aa=as.data.frame(aa)
write_csv(as.data.frame(aa),"D:/Kaggle/HillaryEmail/topicOutput.csv")