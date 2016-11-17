
library(tm)
txt <- system.file("texts", "txt", package = "tm")

(ovid <- VCorpus(DirSource(txt, encoding = "UTF-8"),
                 readerControl = list(language = "lat")))

ovid
ovid[[1]]
ovid[[1,2]] #doesn't work
ovid[[1:2]]
inspect(ovid) #number of docs
lapply(ovid,meta) #some metadata for each doc

#print data - look at data
ovid[[1]]$content #displays lines from first doc
as.character(ovid[[1]]) #does similar to above
writeLines(as.character(ovid[[1]])) #prints without linenumbers
head(as.character(ovid[[1]]),3) #prints set number of lines

as.character(ovid[1]) #prints it funny -DONT USE

#length (ncharacters) of lines
sapply(ovid[[1]]$content,nchar)
sapply(ovid[[2]]$content,nchar)
max(sapply(ovid[[2]]$content,nchar))

writeLines(as.character(ovid[[2]]))


#this doesnt work
for(x in 1:length(ovid)){
  head(as.character(ovid[[x]]),3)
}

lapply(ovid[1:2], as.character)
as.character(ovid[[1]])
ovid[1]


#different dataset
reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- VCorpus(DirSource(reut21578),
                    readerControl = list(reader = readReut21578XMLasPlain))

inspect(reuters) #18 docs
writeLines(as.character(reuters[[1]]))

reuters2 <- tm_map(reuters, stripWhitespace)
writeLines(as.character(reuters2[[1]]))

reuters3 <- tm_map(reuters2, content_transformer(tolower))
writeLines(as.character(reuters3[[1]]))

reuters4 <- tm_map(reuters3, removeWords, stopwords("english"))
writeLines(as.character(reuters4[[1]]))

tm_map(reuters4, stemDocument)
writeLines(as.character(stemDocument[[1]]))


dtm <- DocumentTermMatrix(reuters4)
inspect(dtm[5:10, 740:743])
inspect(dtm[1:5],)
findFreqTerms(dtm, 5) #terms that appear at least 5 times

findAssocs(dtm, "opec", 0.8) #terms correlated with "opec"

#Learning tokenization

data("crude")
inspect(crude)
meta(crude[[1]])
crude[[1]]$content
crude_tok1<-MC_tokenizer(crude[[1]])
scan_tokenizer(crude[[1]])
strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))
strsplit_space_tokenizer(crude[[1]])
library(tau)
tokenize(x)
