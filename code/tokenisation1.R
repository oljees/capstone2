

  library(tm)
#Data to use
list.files("./final/en_US")
setwd("./final/en_US")
#Import data
alldata<-VCorpus((DirSource("./final/en_US")))
inspect(alldata)  #can how many documents and numbero of characters etc.

lapply(alldata,meta) #metadata about all docs in corpus

#length of each document
unlist(lapply(alldata[[1]],length)) 
unlist(lapply(alldata[[2]],length)) 
unlist(lapply(alldata[[3]],length)) 


#Data too large, sample 50%
set.seed(12321)
sample_blogs<-sample(alldata[[1]]$content,20000)
sample_news<-sample(alldata[[2]]$content,20000)
sample_twitter<-sample(alldata[[3]]$content,20000)

sample_file<-paste(sample_blogs,sample_news,sample_twitter)

sample_file<-VCorpus(VectorSource(sample_file))
lapply(sample_file,meta)

#clean and tokenise
z<-tm_map(sample_file,stripWhitespace)
#compare
sample_file[[1]]$content[1:5]
z[[1]]$content[1:5]
s<-sapply(sample_file, strsplit,split=" ")
s<-sapply(s,unlist)
#tokenise sample file

