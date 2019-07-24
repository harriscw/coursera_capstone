

myfunc <- function(myentry="",model="WEIGHTED",myplot="YES"){
  #catch unspecified weights
  if (!(toupper(model) %in% c("EQUAL","WEIGHTED","BACK"))){return("Model weight not valid.  Enter either 'equal', 'weighted', or 'back' ")}else{
    
    #preproc entry
    myentry = sub("^\\s+", "", myentry);myentry = sub("\\s+$", "", myentry)
    myentry=tolower(myentry)
    myentry=gsub("[^A-Za-z ]","",myentry)
    if(substr(myentry,nchar(myentry),nchar(myentry)) != " "){myentry=paste(myentry," ",sep="")}
    
    #split text, ALWAYS add space back in at the end
    mysplit = strsplit(myentry,split=" ")[[1]]
    mysplit[length(mysplit)]=paste(mysplit[length(mysplit)]," ",sep="")
    
    #get matching bigrams
    x = tail(mysplit,1)
    a = head(mydf2[grep(paste("^",x,sep=""),mydf2$ngram),])
    
    #get matching trigrams
    if(length(mysplit)>1){
      y = mysplit[(length(mysplit)-1):length(mysplit)]
      b = head(mydf3[grep(paste("^",y[1]," ",y[2],sep=""),mydf3$ngram),])
      
      #get matching 4grams
      if(length(mysplit)>2){
        z=mysplit[(length(mysplit)-2):length(mysplit)]
        c = head(mydf4[grep(paste("^",z[1]," ",z[2]," ",z[3],sep=""),mydf4$ngram),])
      }else{c=a[0,]}
      
    }else{
      b=a[0,]
      c=a[0,]
    }
    
    #combine them all
    d = rbind(a,b,c)
    
    
    if(nrow(d)>0){
      #loop to get last word for whole words (ie end in a space)
      #for words not ending in a space it goes back to the last space, finds all the characters after that, and deletes that out of the ngram
      # for substringing bigrams
      
      for (i in 1:nrow(d)){
        
        d$suggestion[i]=strsplit(as.character(d$ngram[i])," ")[[1]][length(strsplit(as.character(d$ngram[i])," ")[[1]])]
      }
      
      if(toupper(model)=="WEIGHTED"){
        
        if(length(strsplit(myentry," ")[[1]])<=2){
          d$percentwt[d$type==2]=d$percent[d$type==2]*.25
          d$percentwt[d$type==3]=d$percent[d$type==3]*.75
          
          returndf = aggregate(list(percent=d$percentwt), by=list(suggestion=d$suggestion),FUN=sum, na.rm=TRUE)
          returndf = returndf[order(-returndf$percent),]
          
          
        }else if(length(strsplit(myentry," ")[[1]])>2){
          d$percentwt[d$type==2]=d$percent[d$type==2]*.1
          d$percentwt[d$type==3]=d$percent[d$type==3]*.3
          d$percentwt[d$type==4]=d$percent[d$type==4]*.6
          
          returndf = aggregate(list(percent=d$percentwt), by=list(suggestion=d$suggestion),FUN=sum, na.rm=TRUE)
          returndf = returndf[order(-returndf$percent),]
          
        }
        
      }else if(toupper(model)=="EQUAL"){
        
        returndf = aggregate(list(percent=d$percent), by=list(suggestion=d$suggestion),FUN=sum, na.rm=TRUE)
        returndf$percent=returndf$percent/3
        returndf = returndf[order(-returndf$percent),]
      }else{
        
        #this is the backproposition model
        
        returndf0 = aggregate(list(percent=d$percent), by=list(suggestion=d$suggestion,type=d$type),FUN=sum, na.rm=TRUE)
        returndf0 = returndf0[order(-returndf0$type,-returndf0$percent),]
        
        returndf4=subset(returndf0,returndf0$type==4,c("suggestion","percent","type"))
        returndf3=subset(returndf0,returndf0$type==3,c("suggestion","percent","type"))
        returndf2=subset(returndf0,returndf0$type==2,c("suggestion","percent","type"))
        
        if (nrow(returndf4) > 0){returndf=returndf4}else 
          if (nrow(returndf3) > 0){returndf=returndf3}else 
            if (nrow(returndf2) > 0){returndf=returndf2}else{
              
            }
        
      }
      
      
      # replace swears
      
      badwords = c("fuck","fucks","fucking","fucked","fuckin","shit","shits","bullshit","shitty","cunt","motherfucker","motherfuckers",
                   "bitch","bitches","bitched","bitching","fag","fags","faggot","nigger","niggers","asshole","assholes" )
      
      
      for (i in 1:length(badwords)){
        for (j in 1:length(returndf$suggestion)){
          if(grepl(badwords[i], returndf$suggestion[j])==TRUE){
            returndf$suggestion[j]="[bad word]"
          }
        }
      }
      
      #create a barplot
      if(toupper(myplot)=="YES"){
      returndf$myorder=nrow(returndf):1
      mytable1 = returndf[order(returndf$myorder),]
      returndf$myorder=NULL
      mytable2=as.table(mytable1$percent)
      rownames(mytable2)=mytable1$suggestion
      myplot=barplot(mytable2,horiz=TRUE,las=1,xlab="Percent")

      rownames(returndf)=NULL
      mylist=list(a=returndf,b=myplot)
      }else{
        rownames(returndf)=NULL
        mylist=list(a=returndf,b=NULL)
        
      }
      
      return(mylist)
    }else{
      
      
      mytable1=data.frame(suggestion="the",percent=.0000000001)
      
      if(toupper(myplot)=="YES"){
      mytable2=as.table(mytable1$percent)
      rownames(mytable2)=mytable1$suggestion
      myplot= barplot(mytable2,horiz=T,las=1,xlab="Percent")
      rownames(mytable1)=NULL
      mylist=list(a=mytable1,b=myplot)
      return(mylist)
      }else{
        mylist=list(a=mytable1,b=NULL)
        return(mylist)
        
      }
      
    }
  }
}
