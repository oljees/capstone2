
z<-c("ddidoodooo","balloon","abcd", "fully", "3ede45r", "biiiig", "lllllllll","aaaaaaaaaahhhhhhhhhhhhhhhhhh","aaaeeeeggggg")

grep("(.)*",z)

gsub("\\b(\\S+?)\\1\\S*\\b", "",z)
gsub("(\\S+?)", "",z)

gsub('([[:alpha:]])\\1+', '\\1', 'Buenaaaaaaaaa Suerrrrte')
gsub('([[:alpha:]])\\1+', '\\1', z)

gsub('([[:alpha:]]{1,})\\1+', '\\1', z)

gsub("([[:alpha:]])\\1+","\\1\\1",z)
