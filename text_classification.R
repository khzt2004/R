require(quanteda)#natural language processing package
?quanteda 
require(RColorBrewer)
require(ggplot2)

spam=read.csv("https://raw.githubusercontent.com/khzt2004/R/master/datafiles/spam.csv",header=TRUE, sep=",", quote='\"\"', stringsAsFactors=FALSE)

table(spam$v1)

# add appropriate names to the columns
names(spam)<-c("type","message")
head(spam)

set.seed(2012)
spam<-spam[sample(nrow(spam)),] #randomly shuffling the dataset

?corpus #to search more on this method
msg.corpus<-corpus(spam$message)

docvars(msg.corpus)<-spam$type   #attaching the class labels to the corpus message text

#subsetting only the spam messages
spam.plot<-corpus_subset(msg.corpus,docvar1=="spam")

#now creating a document-feature matrix using dfm()
spam.plot<-dfm(spam.plot, tolower = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_numbers = TRUE, remove=stopwords("SMART"))

spam.col <- brewer.pal(10, "BrBG")  

textplot_wordcloud(spam.plot, min.freq = 16, color = spam.col)  
title("Spam Wordcloud", col.main = "grey14")

ham.plot<-corpus_subset(msg.corpus,docvar1=="ham")
ham.plot<-dfm(ham.plot,tolower = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_numbers = TRUE,remove=c("gt", "lt", stopwords("SMART")))
ham.col=brewer.pal(10, "BrBG")  
textplot_wordcloud(ham.plot,min.freq=50,colors=ham.col,fixed.asp=TRUE)
title("Ham Wordcloud",col.main = "grey14")

# naive bayes classifier
#separating Train and test data
spam.train<-spam[1:4458,]
spam.test<-spam[4458:nrow(spam),]

msg.dfm <- dfm(msg.corpus, tolower = TRUE)  #generating document freq matrix
msg.dfm <- dfm_trim(msg.dfm, min_count = 5, min_docfreq = 3)  
msg.dfm <- dfm_weight(msg.dfm, type = "tfidf") 

head(msg.dfm)

#trining and testing data of dfm 
msg.dfm.train<-msg.dfm[1:4458,]

msg.dfm.test<-msg.dfm[4458:nrow(spam),]

nb.classifier<-textmodel_NB(msg.dfm.train,spam.train[,1])
nb.classifier

pred<-predict(nb.classifier,msg.dfm.test)

#generating a confusion matrix

# use pred$nb.predicted to extract the class labels
table(predicted=pred$nb.predicted,actual=spam.test[,1])

#acccuracy of the classifier on Test data
mean(pred$nb.predicted==spam.test[,1])*100
