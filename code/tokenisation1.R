

library(tm)
#Data to use
list.files("./final/en_US")

#Import data
alldata<-VCorpus((DirSource("./final/en_US")))
inspect(alldata)  #can how many documents and numbero of characters etc.

tokenize(alldata[[1]]$content)
