library(tm)

list.files("./final/en_US")

#1 read in data, maybe only sample?
set.seed(12321)
## Take 10% of each corpus as sample (without replacement) ##
con <- file(description = "./final/en_US/en_US.news.txt", encoding = "UTF-8")
news <- sample(x  = readLines(con = con), size = 0.1*length(readLines(con = con)), replace = FALSE)
close(con)
length(news)

con <- file(description = "./final/en_US/en_US.blogs.txt", encoding = "UTF-8")
blog <- sample(x  = readLines(con = con), size = 0.1*length(readLines(con = con)), replace = FALSE)
close(con)
length(blog)

con <- file(description = "./final/en_US/en_US.twitter.txt", encoding = "UTF-8")
twitter <- sample(x  = readLines(con = con), size = 0.1*length(readLines(con = con)), replace = FALSE)
close(con)
length(twitter)

#maybe save to disk and read sample??

#put into corpus format
corpus<-VCorpus(VectorSource(paste(twitter,blog,news)))


#initial exploration

  #size of each document -can get this from individual docs
  #number of lines per document - again, elements in each doc is line

  #average number of characters per line in document
inspect(corpus[1:5])
corpus[[1]]$meta
corpus[[1]]$content

lapply(corpus[1:3],as.character) #first three

meta(corpus) #too long
length(corpus)

corpus[[max(length(corpus))]]$meta #last document
corpus[[max(length(corpus))]]$content #last document

  #any date information, i.e meta data exploration
corpus[[1]]$meta$datetimestamp

  #any author information-not many  

#######################
#clean data
#######################
corpus[[1]]$content

#punctuation
corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]$content

#numbers
grep("[0-9]",unlist(lapply(corpus[1:5],as.character)),value=TRUE) #shows 3 and 4 have numbers
unlist(lapply(corpus[3:4],as.character))
#note you lose n-grams such as "books a 6am flight" to "books a am flight"

#for this project would be good to replace numbers with number range for prediction
corpus<-tm_map(corpus,removeNumbers)
grep("[0-9]",unlist(lapply(corpus[1:5],as.character)),value=TRUE)

#white space
corpus <- tm_map(corpus, stripWhitespace)
unlist(lapply(corpus[3:4],as.character)) #dodn't notice difference

#lower case
corpus <- tm_map(corpus, content_transformer(tolower))
unlist(lapply(corpus[3:4],as.character)) 

#remove profanity words
# from http://www.bannedwordlist.com/
download.file("http://www.bannedwordlist.com/",
              "swearWords.csv", method = "curl")
profanity<-read.csv("swearWords.csv",stringsAsFactors = F,header = F)
profanity<-as.character(profanity)

corpus <- tm_map(corpus, removeWords, profanity)

#special characters

for(j in seq(corpus)){
  corpus[[j]]<-gsub("''","",corpus[[j]])
}

#white space remove extra whitespace reulting from removed words
corpus <- tm_map(corpus, stripWhitespace)
unlist(lapply(corpus[3:4],as.character)) #

#treat corpus as text document
corpus <- tm_map(corpus, PlainTextDocument) 

###############################################
#secondary exploration
###############################################

#Document Term Matrix
dtmatrix <- DocumentTermMatrix(corpus) #this has docs in rows and terms as columns
tdmatrix <- TermDocumentMatrix(corpus) #this is the transpose of above, terms as rows, docs as cols

inspect(dtmatrix[200:210,10020:10030])
dim(dtmatrix) #docs,terms  

#most common words (by document?)
common100<-findFreqTerms(dtmatrix, lowfreq=100) #maybe keep only these?


  
#dtm<-DocumentTermMatrix(corpus,list(dictionary=common100))  #takes way too long

#remove any sparse terms
#these will remove terms that are not in 40% of documents


removeSparseTerms(dtmatrix, 0.4)
inspect(dtmatrix[1:5,1:5]) #dodgy still shows up?

#most frequent terms in doc term matrix



#look at terms most commonly correlated with most popular terms r>0.8

