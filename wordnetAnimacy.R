wordnetAnimacy<-function(word)
{
  require(RCurl)
  require(XML)
  
  load("database.rdata")
  database$word<-as.character(database$word)
  database$type<-as.character(database$type)
if(!any(which(database[, "word"]==word)))  
{
  url<-paste("http://wordnetweb.princeton.edu/perl/webwn?s=", word,"&sub=Search+WordNet&o2=1&o0=1&o8=1&o1=1&o7=1&o5=1&o9=&o6=1&o3=1&o4=1&h=0", sep="", collapse="")
  doc <- htmlParse(getURL(url,.encoding="UTF-8" ))
  scraped <- lapply(doc['//ul'],xmlValue)
  scraped<-tolower(scraped)
  type<-str_extract(scraped, "<noun.[a-z]+>")
  type<-type[1]
  type<-gsub("<", "", type)
  type<-gsub(">", "", type)

  if(length(type)>0)
  {
    new<-data.frame(word=as.character(word), type=as.character(type), stringsAsFactors = F)
    rbind(new, database)->database
    save(database, file="database.rdata")
  }
  else
  {
    type<-"NA"
  }
  
}
 else
 {
   if(!is.na(database[database$word==word, "type",]))
   {
     
   type<-database[database$word==word, "type"]
   }
   else
   {
    type<-"NA" 
   }
   
 }
  return (type)
  
}