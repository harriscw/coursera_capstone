library(qdap)
library(R.utils)

####################################################
# First read in data that wasn't used for the model
####################################################

### Read in data

con <- file("final/en_US/en_US.twitter.txt", "r")
y1=readLines(con)
close(con)

# Sample this down to even out word count from all sources

set.seed(1122)
for (i in 1:length(y1)){
  if (i%%50000==0){print(i)}
  z = runif(1)
  if(z<=.02){y1[i]=NA}
}

y1[1:10]
y1 = y1[!is.na(y1)]
length(y1)

y1=sample(y1,1000)


con <- file("final/en_US/en_US.blogs.txt", "r")
y2=readLines(con)
close(con)


# Sample this down to even out word count from all sources
set.seed(3253)
for (i in 1:length(y2)){
  if (i%%50000==0){print(i)}
  z = runif(1)
  if(z<=.02){y2[i]=NA}
}

y2[1:10]
y2 = y2[!is.na(y2)]
length(y2)

y2=sample(y2,1000)


con <- file("final/en_US/en_US.news.txt", "r")
y3=readLines(con)
close(con)

set.seed(3883)
for (i in 1:77259){
  if (i%%5000==0){print(i)}
  z = runif(1)
  if(z<=.3){y3[i]=NA}
}

y3[1:10]
y3 = y3[!is.na(y3)]
length(y3)

y3=sample(y3,1000)



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

#final dataset with random sample from 3 different sources
mytest=c(y1,y2,y3)
mytest=mytest[mytest!=""]
head(mytest)

################################################################
## End of sampling and processing
## Now remove last word, create var for last word for comparison
##
## This part is slow
##
################################################################


lastword=rep(NA,length(mytest))
for (i in 1:length(mytest)){
  print(i)
  lastword[i]=strsplit(mytest,split=" ")[[i]][length(strsplit(mytest,split=" ")[[i]])]
}

mytest2=data.frame(mytest,lastword)
head(mytest2)

lastwordrem=rep(NA,nrow(mytest2))
for (i in 1:nrow(mytest2)){
  print(i)
  lastwordrem[i]=substr(mytest2$mytest[i],1,tail(gregexpr(mytest2$lastword[i],mytest2$mytest[i])[[1]][1],1)-1)
}

mytest2=data.frame(mytest2,lastwordrem)
head(mytest2)

##############################
# Get back prop predicted word
##############################

backpred=data.frame(a=rep(NA,nrow(mytest2)),b=rep(NA,nrow(mytest2)))
for (i in 1:nrow(mytest2)){
  print(i)
  backpred$back1[i]=myfunc(as.character(mytest2$lastwordrem[i]),model="BACK",myplot="NO")$a[1,1]
  backpred$backall[i]=paste(myfunc(as.character(mytest2$lastwordrem[i]),model="BACK",myplot="NO")$a[,1],sep="",collapse=" ")
}

mytest2=data.frame(mytest2,backpred)
names(mytest2)
head(mytest2)[,c("lastword", "back1","backall")]



##############################
# Get weighted predicted word
##############################

weightpred=data.frame(weight1=rep(NA,nrow(mytest2)),weightall=rep(NA,nrow(mytest2)))
for (i in 1:nrow(mytest2)){
  print(i)
  weightpred$weight1[i]=myfunc(as.character(mytest2$lastwordrem[i]),model="WEIGHTED",myplot="NO")$a[1,1]
  weightpred$weightall[i]=paste(myfunc(as.character(mytest2$lastwordrem[i]),model="WEIGHTED",myplot="NO")$a[,1],sep="",collapse=" ")
}

mytest2=data.frame(mytest2,backpred)
names(mytest2)
head(mytest2)[,c("lastword", "weight1","weightall")]



##############################
# Get equal predicted word
##############################

equalpred=data.frame(equal1=rep(NA,nrow(mytest2)),equalall=rep(NA,nrow(mytest2)))
for (i in 1:nrow(mytest2)){
  print(i)
  equalpred$equal1[i]=myfunc(as.character(mytest2$lastwordrem[i]),model="EQUAL",myplot="NO")$a[1,1]
  equalpred$equalall[i]=paste(myfunc(as.character(mytest2$lastwordrem[i]),model="EQUAL",myplot="NO")$a[,1],sep="",collapse=" ")
}

mytest2=data.frame(mytest2,equalpred)
names(mytest2)
head(mytest2)[,c("lastword", "equal1","equalall")]

names(mytest2)
# Error rate for list of suggested words

backsum=0
for(i in 1:nrow(mytest2)){
  backsum=backsum +grepl(as.character(mytest2$lastword[i]),as.character(mytest2$b[i]))
}

weightsum=0
for(i in 1:nrow(mytest2)){
  weightsum=weightsum +grepl(as.character(mytest2$lastword[i]),as.character(mytest2$weightall[i]))
}

equalsum=0
for(i in 1:nrow(mytest2)){
  equalsum=equalsum +grepl(as.character(mytest2$lastword[i]),as.character(mytest2$equalall[i]))
}

# backsum/nrow(mytest2)
# 0.1598265

# weightsum/nrow(mytest2)
# 0.1765098

# equalsum/nrow(mytest2)



# get accuracy rate
sum(as.character(mytest2$lastword)==as.character(mytest2$backpred))/nrow(mytest2) #0.07407407
sum(as.character(mytest2$lastword)==as.character(mytest2$weightpred))/nrow(mytest2) #0.07340674
sum(as.character(mytest2$lastword)==as.character(mytest2$equalpred))/nrow(mytest2) #0.07474141

# get differences among predicted words
sum(as.character(mytest2$backpred)==as.character(mytest2$weightpred))/nrow(mytest2) #0.9733066
sum(as.character(mytest2$backpred)==as.character(mytest2$equalpred))/nrow(mytest2) #0.9566233
sum(as.character(mytest2$equalpred)==as.character(mytest2$weightpred))/nrow(mytest2) #0.9833166

# df where pred differs
head(mytest2[as.character(mytest2$weightpred) != as.character(mytest2$equalpred) |
               as.character(mytest2$weightpred) != as.character(mytest2$backpred) |
               as.character(mytest2$backpred) != as.character(mytest2$equalpred)
             ,c("mytest","backpred","equalpred", "weightpred")])

myfunc("have a good",model="equal")
myfunc("have a good",model="weighted")
myfunc("have a good",model="back")

