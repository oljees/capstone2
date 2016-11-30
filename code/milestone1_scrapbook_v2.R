library(tm)
library(dplyr)
library(ggplot2)
library(SnowballC)

list.files("./final/en_US")

#1 read in data, maybe only sample?
set.seed(12321)
## Take 5% of each corpus as sample (without replacement) ##
con <- file(description = "./final/en_US/en_US.news.txt", encoding = "UTF-8")
news <- sample(x  = readLines(con = con), size = 0.05*length(readLines(con = con)), replace = FALSE)
close(con)
length(news)

con <- file(description = "./final/en_US/en_US.blogs.txt", encoding = "UTF-8")
blog <- sample(x  = readLines(con = con), size = 0.05*length(readLines(con = con)), replace = FALSE)
close(con)
length(blog)

con <- file(description = "./final/en_US/en_US.twitter.txt", encoding = "UTF-8")
twitter <- sample(x  = readLines(con = con), size = 0.05*length(readLines(con = con)), replace = FALSE)
close(con)
length(twitter)

#save to disc for later
if (!dir.exists("./final/en_US/samples/")){
  dir.create("./final/en_US/samples/")
}

con<-file(description = "./final/en_US/samples/en_US.blogs.txt")
writeLines(blog,con)
close(con)

con<-file(description = "./final/en_US/samples/en_US.twitter.txt")
writeLines(twitter,con)
close(con)

con<-file(description = "./final/en_US/samples/en_US.news.txt")
writeLines(news,con)
close(con)

#######################################
#LOAD SAMPLE
####################################

#now clear workspace and read in only sample data
docs<-VCorpus((DirSource("./final/en_US/samples")))
docs
inspect(docs)
lapply(docs,meta)
#now as 3 corpora

#view last one
writeLines(as.character(docs[[2]]))

#this can be used to convert certain characters to spaces
#note it is wrapped in the content transformer function
toSpace <- content_transformer(
  function(x, pattern) {
    return (gsub(pattern, " ", x))
  }
)

#Remove punctuation – replace punctuation marks with ” “
writeLines(as.character(docs[[2]])) #view twitter


docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, toSpace, "…") 
docs <- tm_map(docs, toSpace, "–") 
docs <- tm_map(docs, toSpace, "-") 
docs <- tm_map(docs, toSpace, ":") 
docs <- tm_map(docs, toSpace, "'") 
docs <- tm_map(docs, toSpace, "'") 
docs <- tm_map(docs, toSpace, " -")
docs<- tm_map(docs,toSpace,"”")
docs<- tm_map(docs,toSpace,"‘") #doesn't work
docs<- tm_map(docs,toSpace,"’")
docs<- tm_map(docs,toSpace,"’")
docs<- tm_map(docs,toSpace,"–")

#remove any word where character repeated more than 3 times?
# () captures a letter first, \\1 refers to that letter, + means to match 
# it once or more; put all these pieces together, we can match a letter two or more times.
# 
# To include other characters besides alphanumerics, replace [[:alpha:]] with a regex 
# matching whatever you wish to include.
#gsub('([[:alpha:]])\\1+', '\\1', z)
remove_rpt <- content_transformer(
  function(x, pattern) {
    return (gsub('([[:alpha:]])\\1+', '\\1\\1', x))
  }
)

docs<-tm_map(docs,remove_rpt)


#ok,so still a bit of punctuation in there - not sure why, maybe encoding?

#Transform to lower case (need to wrap in content_transformer)
docs <- tm_map(docs,content_transformer(tolower))

#Strip digits (std transformation, so no need for content_transformer)
docs <- tm_map(docs, removeNumbers)

#remove stopwords using the standard list in tm
#not doing this as may want to predict stop words
#docs <- tm_map(docs, removeWords, stopwords("english"))

#remove profanity words
# from http://www.bannedwordlist.com/
download.file("http://www.bannedwordlist.com/",
              "swearWords.csv", method = "curl")
profanity<-read.csv("swearWords.csv",stringsAsFactors = F,header = F)
profanity<-as.character(profanity)

docs <- tm_map(docs, removeWords, profanity)

#remove everything else that is not alphanum
# removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
# docs <- tm_map(docs, removeSpecialChars)

removeSpecialChars <- content_transformer(
  function(x, pattern) {
    return (gsub("[^a-zA-Z0-9 ]","",x))
  }
)
docs<-tm_map(docs,removeSpecialChars)

#Strip whitespace (cosmetic?)
docs <- tm_map(docs, stripWhitespace)


#check
writeLines(as.character(docs[[2]]))

#treat corpus as text document
#docs_bu<-docs
#docs <- tm_map(docs, PlainTextDocument) 

#Don't neccesserily want to stem or lemmatise either
dtm <- DocumentTermMatrix(docs)

#maybe only keep words bigger than 2 chars and those in more than 2 of the three docs
#note takes about 10mins to run
#based on this will reduce samplesize to 5%
#dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20),
#                                             bounds = list(global = c(1,2))))

inspect(dtm[1:2,1000:1005])
dtm #190993 terms in 3 docs at 10% sample, 113913 at 5% sample
dim(dtm)

#probably need to cut this down a bit

#but before that, try calculating frequency of each
freq <- colSums(as.matrix(dtm))
doc_n<-as.data.frame(rowSums(as.matrix(dtm)))
names(doc_n)<-"n-terms"
length(freq)

#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#inspect most frequently occurring terms
freq[head(ord,20)]

#inspect least frequently occurring terms
freq[tail(ord,20)]

#cool!

#return all terms that occur more than 80 times in the entire corpus
#Note, however, that the result is ordered alphabetically, not by frequency
findFreqTerms(dtm,lowfreq=1000)

#check per doc
#blogs
freq_blogs <- colSums(as.matrix(dtm[1,]))
length(freq_blogs)
ord_blogs <- order(freq_blogs,decreasing=TRUE)
top_blogs<-as.data.frame(freq_blogs[head(ord_blogs,20)])
names(top_blogs)<-"count"

#news
freq_news <- colSums(as.matrix(dtm[2,]))
length(freq_news)
ord_news <- order(freq_news,decreasing=TRUE)
freq_news[head(ord_news,20)]
top_news<-as.data.frame(freq_news[head(ord_news,20)])
names(top_news)<-"count"

#twitter
freq_twitter <- colSums(as.matrix(dtm[3,]))
length(freq_twitter)
ord_twitter <- order(freq_twitter,decreasing=TRUE)
freq_twitter[head(ord_twitter,20)]
top_twitter<-as.data.frame(freq_twitter[head(ord_twitter,20)])
names(top_twitter)<-"count"

#check some word associations
#random from most common words above
findAssocs(dtm,"able",0.8)
#here really hiogh number of high correlation >0.97
#maybe because only three documents to check? more words per doc?
#not useful
#need to do per sentence maybe?

#######
#Some graphics
#######
#create data frame with words and counts
wf=data.frame(term=names(freq),occurrences=freq)
library(ggplot2)
#plot for words with count>100
#do a barchart
p <- ggplot(subset(wf, freq>10000), aes(reorder(term,-occurrences), occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))+
  ggtitle("Terms occuring >10000 times in all docs")+
  xlab("Terms")
p

ggplot(top_blogs,aes(reorder(row.names(top_blogs),-count),count))+
  geom_bar(stat="identity")+
  ggtitle("Top 20 terms in blogs")+
  xlab("Terms")

ggplot(top_news,aes(reorder(row.names(top_news),-count),count))+
  geom_bar(stat="identity")+
  ggtitle("Top 20 terms in news")+
  xlab("Terms")

ggplot(top_twitter,aes(reorder(row.names(top_twitter),-count),count))+
  geom_bar(stat="identity")+
  ggtitle("Top 20 terms in twitter")+
  xlab("Terms")


#now create some n-grams and find some of the most common
library("RWeka")
library("tm")

#create bigram function
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

bigram_dtm<- DocumentTermMatrix(docs, control = list(tokenize = BigramTokenizer))
dim(bigram_dtm)
inspect(bigram_dtm[1:3,150000:150010])

freq_bigram <- colSums(as.matrix(bigram_dtm))
doc_n_bigram<-as.data.frame(rowSums(as.matrix(bigram_dtm)))
names(doc_n_bigram)<-"n-bigrams"
length(freq_bigram)

#create sort order (descending)
ord_bigram <- order(freq_bigram,decreasing=TRUE)
#inspect most frequently occurring terms
freq_bigram[head(ord_bigram,20)]

top_bigrams<-as.data.frame(freq_bigram[head(ord_bigram,20)])
names(top_bigrams)<-"count"
#plot common bigrams
ggplot(top_bigrams,aes(reorder(row.names(top_bigrams),-count),count))+
  geom_bar(stat="identity")+
  ggtitle("Top 20 bigrams in data")+
  xlab("bi-grams")

#-------
#create trigram function
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

trigram_dtm<- DocumentTermMatrix(docs, control = list(tokenize = TrigramTokenizer))
dim(trigram_dtm)
inspect(trigram_dtm[1:3,150000:150010])

freq_trigram <- colSums(as.matrix(trigram_dtm))
doc_n_trigram<-as.data.frame(rowSums(as.matrix(trigram_dtm)))
names(doc_n_trigram)<-"n-trigrams"
length(freq_trigram)

#create sort order (descending)
ord_trigram <- order(freq_trigram,decreasing=TRUE)
#inspect most frequently occurring terms
freq_trigram[head(ord_trigram,20)]

top_trigrams<-as.data.frame(freq_trigram[head(ord_trigram,20)])
names(top_trigrams)<-"count"
#plot common bigrams
ggplot(top_trigrams,aes(reorder(row.names(top_trigrams),-count),count))+
  geom_bar(stat="identity")+
  ggtitle("Top 20 trigrams in data")+
  xlab("tri-grams")+
  theme(axis.text.x=element_text(angle=45, hjust=1))
