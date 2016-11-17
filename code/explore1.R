
library(tm)
#Data to use
list.files("./final/en_US")

#Import data
alldata<-VCorpus((DirSource("./final/en_US")))
inspect(alldata)  #can how many documents and numbero of characters etc.

meta(alldata[[1]]) #some metadata about each document
lapply(alldata,meta) #metadata about all docs in corpus


head(as.character(alldata[[2]]),5)

#length of longest line in any dataset
length(alldata) #3 docs

#this counts number of characters in each line and
#extracts the maximum for each doc
max(sapply(alldata[[1]]$content,nchar))
max(sapply(alldata[[2]]$content,nchar))
max(sapply(alldata[[3]]$content,nchar))

#1 is blogs so it has the longest 40835

#######Quiz1 Q4
#In the en_US twitter data set, 
# if you divide the number of lines where the word 
# "love" (all lowercase) occurs by the number of 
# lines the word "hate" (all lowercase) occurs, about what do you get?

#note twitter is alldata[[3]]
alldata[[3]]$meta$id
head(alldata[[3]]$content,10)

#number of lines with love in lower case
grep("you",head(alldata[[3]]$content,10))
length(grep("you",head(alldata[[3]]$content,10)))

love_nl<-length(grep("love",alldata[[3]]$content))
hate_nl<-length(grep("hate",alldata[[3]]$content))
love_nl/hate_nl

###----------
#QUIZ1 Quest5
#The one tweet in the en_US twitter data set that matches the word "biostats" says what?

grep("biostats", alldata[[3]]$content, value=TRUE)
#[1] "i know how you feel.. i have biostats on tuesday and i have yet to study =/"

###---QUIZ1 Quest 6
#How many tweets have the exact characters 
# "A computer once beat me at chess, but it was no match for me at kickboxing". 
# (I.e. the line matches those characters exactly.)

grep("A computer once beat me at chess, but it was no match for me at kickboxing",alldata[[3]]$content, value=TRUE)

#delete this line

