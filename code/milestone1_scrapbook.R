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

#clean data
corpus[[1]]$content

#punctuation
corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]$content

#numbers
grep("[0-9]",unlist(lapply(corpus[1:5],as.character)),value=TRUE) #shows 3 and 4 have numbers
unlist(lapply(corpus[3:4],as.character))
#note you lose n-grams such as "books a 6am flight" to "books a am flight"

#for this project would be good to replace numbers with number range for prediction

#special characters

#white space
corpus <- tm_map(corpus, stripWhitespace)
unlist(lapply(corpus[3:4],as.character)) #dodn't notice difference

#lower case
corpus <- tm_map(corpus, content_transformer(tolower))
unlist(lapply(corpus[3:4],as.character)) #dodn't notice difference

#split into words (tokenisation)

#remove profanity words
# from http://www.bannedwordlist.com/
download.file("http://www.bannedwordlist.com/",
              "swearWords.csv", method = "curl")
profanity<-read.csv("swearWords.csv",stringsAsFactors = F,header = F)
profanity<-as.character(profanity)

corpus <- tm_map(corpus, removeWords, profanity)

#secondary exploration
#Document Term Matrix
dtmatrix <- DocumentTermMatrix(corpus)

  #most common words (by document?)
findFreqTerms(dtmatrix, lowfreq=1000)
  
  #least common words

#remove any sparse terms
#these will remove terms that are not in 40% of documents

inspect(dtmatrix[100:105,100:105]) #shows lots of 0s - sparse terms
inspect(dtmatrix[1:5,1:5]) #shows some dodgy characters

removeSparseTerms(dtmatrix, 0.4)
inspect(dtmatrix[1:5,1:5]) #dodgy still shows up?

#most frequent terms in doc term matrix



#look at terms most commonly correlated with most popular terms r>0.8

