library(qdap)
library(R.utils)

### Read in data

con <- file("final/en_US/en_US.twitter.txt", "r")
y1=readLines(con)
close(con)

# Sample this down to even out word count from all sources

set.seed(1122)
for (i in 1:2360148){
  if (i%%50000==0){print(i)}
  z = runif(1)
if(z>.02){y1[i]=NA}
}

y1[1:10]
y1 = y1[!is.na(y1)]
length(y1)



con <- file("final/en_US/en_US.blogs.txt", "r")
y2=readLines(con)
close(con)


# Sample this down to even out word count from all sources
set.seed(3253)
for (i in 1:899288){
  if (i%%50000==0){print(i)}
  z = runif(1)
  if(z>.02){y2[i]=NA}
}

y2[1:10]
y2 = y2[!is.na(y2)]
length(y2)


con <- file("final/en_US/en_US.news.txt", "r")
y3=readLines(con)
close(con)

set.seed(3883)
for (i in 1:77259){
  if (i%%5000==0){print(i)}
  z = runif(1)
  if(z>.3){y3[i]=NA}
}

y3[1:10]
y3 = y3[!is.na(y3)]
length(y3)



############################
# Do some processing
############################

grep("<U+",y1) #No weird unicode things in my sample

y1=gsub("[^A-Za-z ]","",y1)

# remove symbols and numbers
y1=gsub("[^A-Za-z ]","",y1)
# make everything lower case
y1=tolower(y1)
# trim leading and trailing whitespace
y1 = sub("^\\s+", "", y1);y1 = sub("\\s+$", "", y1)


# remove symbols and numbers
y2=gsub("[^A-Za-z ]","",y2)
# make everything lower case
y2=tolower(y2)
# trim leading and trailing whitespace
y2 = sub("^\\s+", "", y2);y2 = sub("\\s+$", "", y2)

# remove symbols and numbers
y3=gsub("[^A-Za-z ]","",y3)
# make everything lower case
y3=tolower(y3)
# trim leading and trailing whitespace
y3 = sub("^\\s+", "", y3);y3 = sub("\\s+$", "", y3)


############################
# preliminary word count/length analysis
############################

# sum(wc(y1),na.rm = T) #7350107
# sum(wc(y2),na.rm = T) #4455970
# sum(wc(y3),na.rm = T) #2573400
# 
# y1[1:5]
# y2[1:5]
# y3[1:5]

############################
# Summary stats
############################

# 
# mydata = c("twitter","blogs","news")
# mywc = c(median(wc(y1),na.rm=T),median(wc(y2),na.rm=T),median(wc(y3),na.rm=T))
# mycharmed = c(median(nchar(y1)),median(nchar(y2)),median(nchar(y3)))
# mylen = c(length(y1),length(y2),length(y3))
# mywctot = c(sum(wc(y1),na.rm=T),sum(wc(y2),na.rm=T),sum(wc(y3),na.rm=T))
# mydf = data.frame(mydata,mywc,mychar,mylen,mywctot)
# mydf = data.frame(mydata,mylen,mycharmed)
# mydf
# 
# par(mfrow=c(2,3))
# hist(nchar(y1))
# hist(nchar(y2))
# hist(nchar(y3))
# hist(wc(y1))
# hist(wc(y2))
# hist(wc(y3))

###############################
# Count lines in each file
###############################

# twitter is 2360148
# blogs is 899288
# news is 77259













# replace swears
# grep("fuck",x)


# badwords = c("fuck","fucking","fucked","fuckin","shit","shits","bullshit","shitty","cunt",
#              "bitch","bitches","bitched","bitching","fag","fags","faggot","nigger","niggers","asshole","assholes" )
# 
# for (i in 1:length(badwords)){
#   print(badwords[i])
#   y=gsub("asshole","[bad word]",y)
# }
# 
# zz=grep("asshole",y)
# for (i in 1:length(zz)){
#   print(y[zz[i]])
# }


