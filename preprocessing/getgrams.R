library(ngram)

source("preproc.R")


x = c(y1,y2,y3)
rm(list=setdiff(ls(), "x"))


  #############
  # get bigrams
  #############
  
  mybigrams = NULL
  longbigrams = NULL
  longerbigrams = NULL
  
for (i in 1:length(x)){
  
  if (i==1){print(paste("total records: ",length(x),sep=""))}
  if (i%%1000==0){print(i)}
  
  if(sapply(gregexpr("\\W+", x[i]), length)>1){
      ng = ngram(x[i], n=2)
      a = get.ngrams(ng)
      a = unique(a)
      mybigrams = c(mybigrams,a)
      
      # this dumps off list of bigrams off to a longer list every 5000 records to speed things up
      if(i%%5000==0){
        longbigrams = c(longbigrams,mybigrams)
        mybigrams=NULL
        
        # this dumps off list of bigrams off to an even longer list every 50000 records to speed things up
        # only evaluated within the previous if statement
        if(i%%20000==0){
          longerbigrams = c(longerbigrams,longbigrams)
          longbigrams=NULL
        }
      }
  }
  
}
  
#get leftovers
  length(mybigrams)
  length(longbigrams)
  longerbigrams = c(longerbigrams,longbigrams,mybigrams)

  
################  
#get frequencies
################  

  mydf2 = data.frame(table(longerbigrams))
  mydf2$bigrams=as.character(mydf2$longerbigrams)
  mydf2$longerbigrams=NULL
  mydf2=mydf2[order(-mydf2$Freq),]
  
  # want bigram frequency over frequency of first word in bigram
  # Get first word in bigram
  mydf2$first.word=unlist(lapply(strsplit(mydf2$bigrams," "), '[[', 1))

  # aggregate to get frequency of first word for denominator
  mydf2b = aggregate(list(denom=mydf2$Freq), by=list(first.word=mydf2$first.word),FUN=sum, na.rm=TRUE)
  
  #merge back in
  mydf2b=mydf2b[order(-mydf2b$denom),]
    
    mydf2 = merge(mydf2,mydf2b,by="first.word")
    mydf2$percent=mydf2$Freq/mydf2$denom
    mydf2=mydf2[order(-mydf2$percent),]
    head(mydf2)
    mydf2$first.word=NULL
    rownames(mydf2)=NULL
    
    mydf2 = subset(mydf2,mydf2$Freq>1)
    rm(list=setdiff(ls(), c("x","mydf2")))

  
  #############
  # get trigrams
  #############
  
  mytrigrams = NULL
  longtrigrams = NULL
  longertrigrams = NULL
  
  for (i in 1:length(x)){
    
    if (i==1){print(paste("total records: ",length(x),sep=""))}
    if (i%%1000==0){print(i)}
    
    if(sapply(gregexpr("\\W+", x[i]), length)>2){
      ng = ngram(x[i], n=3)
      a = get.ngrams(ng)
      a = unique(a)
      mytrigrams = c(mytrigrams,a)
      
      # this dumps off list of trigrams off to a longer list every 5000 records to speed things up
      if(i%%5000==0){
        longtrigrams = c(longtrigrams,mytrigrams)
        mytrigrams=NULL
        
        # this dumps off list of trigrams off to an even longer list every 50000 records to speed things up
        # only evaluated within the previous if statement
        if(i%%20000==0){
          longertrigrams = c(longertrigrams,longtrigrams)
          longtrigrams=NULL
        }
      }
    }
    
  }
  
  #get leftovers
  length(mytrigrams)
  length(longtrigrams)
  longertrigrams = c(longertrigrams,longtrigrams,mytrigrams)
  # longertrigrams=longertrigrams[1:10000]
  
  rm(list=setdiff(ls(), c("x","mydf2","longertrigrams")))
  
    #get frequencies
  mydf3 = data.frame(table(longertrigrams))

  mydf3$trigrams=as.character(mydf3$longertrigrams)
  mydf3$longertrigrams=NULL
  mydf3=mydf3[order(-mydf3$Freq),]
  head(mydf3)
  

  mydf3 = subset(mydf3,mydf3$Freq>1)
  rm(list=setdiff(ls(), c("x","mydf2","mydf3")))
  
  # want triigram frequency over frequency of first two words in trigram
  # Get first two words in trigram
  mydf3$first.two.words=paste(unlist(lapply(strsplit(mydf3$trigrams," "), '[[', 1)),unlist(lapply(strsplit(mydf3$trigrams," "), '[[', 2)),sep=" ")
  
  # aggregate to get frequency of first word for denominator
  mydf3b = aggregate(list(denom=mydf3$Freq), by=list(first.two.words=mydf3$first.two.words),FUN=sum, na.rm=TRUE)
  
  #merge back in
  mydf3b=mydf3b[order(-mydf3b$denom),]

  mydf3 = merge(mydf3,mydf3b,by="first.two.words")
  mydf3$percent=mydf3$Freq/mydf3$denom
  mydf3=mydf3[order(-mydf3$percent),]
  head(mydf3)
  mydf3$first.two.words=NULL
  rownames(mydf3)=NULL
  
  rm(list=setdiff(ls(), c("x","mydf2","mydf3")))
  
  
  
  
  
  
  
  
  
  
  
  
  #############
  # get fourgrams
  #############
  
  myfourgrams = NULL
  longfourgrams = NULL
  longerfourgrams = NULL
  
  for (i in 1:length(x)){
    
    if (i==1){print(paste("total records: ",length(x),sep=""))}
    if (i%%1000==0){print(i)}
    
    if(sapply(gregexpr("\\W+", x[i]), length)>3){
      ng = ngram(x[i], n=4)
      a = get.ngrams(ng)
      a = unique(a)
      myfourgrams = c(myfourgrams,a)
      
      # this dumps off list of fourgrams off to a longer list every 5000 records to speed things up
      if(i%%5000==0){
        longfourgrams = c(longfourgrams,myfourgrams)
        myfourgrams=NULL
        
        # this dumps off list of fourgrams off to an even longer list every 50000 records to speed things up
        # only evaluated within the previous if statement
        if(i%%20000==0){
          longerfourgrams = c(longerfourgrams,longfourgrams)
          longfourgrams=NULL
        }
      }
    }
    
  }
  
  #get leftovers
  length(myfourgrams)
  length(longfourgrams)
  longerfourgrams = c(longerfourgrams,longfourgrams,myfourgrams)
  
  rm(list=setdiff(ls(), c("x","mydf2","mydf3","longerfourgrams")))
  
  #get frequencies
  mydf4 = data.frame(table(longerfourgrams))
  mydf4$fourgrams=as.character(mydf4$longerfourgrams)
  mydf4$longerfourgrams=NULL
  mydf4=mydf4[order(-mydf4$Freq),]
  head(mydf4)
  

  mydf4 = subset(mydf4,mydf4$Freq>1)
  rm(list=setdiff(ls(), c("x","mydf2","mydf3","mydf4")))
  
  # want trigram frequency over frequency of first two words in trigram
  # Get first two words in trigram
  mydf4$first.three.words=paste(unlist(lapply(strsplit(mydf4$fourgrams," "), '[[', 1)),unlist(lapply(strsplit(mydf4$fourgrams," "), '[[', 2)),unlist(lapply(strsplit(mydf4$fourgrams," "), '[[', 3)),sep=" ")
  head(mydf4)
  # aggregate to get frequency of first word for denominator
  mydf4b = aggregate(list(denom=mydf4$Freq), by=list(first.three.words=mydf4$first.three.words),FUN=sum, na.rm=TRUE)
  
  #merge back in
  mydf4b=mydf4b[order(-mydf4b$denom),]
  
  mydf4 = merge(mydf4,mydf4b,by="first.three.words")
  mydf4$percent=mydf4$Freq/mydf4$denom
  mydf4=mydf4[order(-mydf4$Freq),]
  head(mydf4)
  mydf4$first.three.words=NULL
  rownames(mydf4)=NULL
  
  rm(list=setdiff(ls(), c("mydf2","mydf3","mydf4")))
  
  names(mydf2)[2] = "ngram"
  names(mydf3)[2] = "ngram"
  names(mydf4)[2] = "ngram"
  
  mydf2$type=2
  mydf3$type=3
  mydf4$type=4
  

  
  
  