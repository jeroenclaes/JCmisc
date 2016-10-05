fetchParsedTweets<-function(api_secret, token, token_secret, query, excludedids=NULL,loc, radius, ntweets=10000, parserPath, parserModel)
{
  options(warn=-1)
  
  #Function to fetch tweets with the search api.
  
  #api_secret, token, token_secret: register for an account at developer.twitter.com, and request api_secret, token, and token scret for a new web application. Fill in those details there.  
  
  #'query' should be a character string that separates the query terms with a comma.
  #'excludedids': a vector of tweet ids not to be included
  #'loc': location e.g., Seville, Spain
  #'radius': a number  describing the radius (in miles) around which the script should search e.g., 10 for 10 miles around Seville, Spain
  #'ntweets': how many tweets? 10K appears to be the limit

  #parserPath: location on your hard drive where you have the main file of the Stanford Tagger (stanford-postagger.jar). E.g., parserPath="/Users/jeroenclaes/pos_tagger/stanford-postagger.jar"
  #parserModel: location on your hard drive where you have the parser model you want to use: e.g., parserModel="/Users/jeroenclaes/pos_tagger/models/english-left3words-distsim.tagger"  
  
  require(twitteR)
  require(RCurl) 
  require(ROAuth)
  require(base64enc)
  require(stringr)
  require(plyr)
  require(Hmisc)
  require(XML)
  require(stringi)
  
  #Create Twitter Connection
  setup_twitter_oauth(api_key, api_secret, NULL, NULL)
  
  #embedded function for geocoding
  geocodeAdddress <- function(address, radius) {
    require(RJSONIO)
    url <- "http://maps.google.com/maps/api/geocode/json?address="
    url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
    x <- fromJSON(url, simplify = FALSE)
    if (x$status == "OK") {
      out <- paste(x$results[[1]]$geometry$location$lat,",", 
                   x$results[[1]]$geometry$location$lng,",", radius,"mi", sep="", collapse="")
    } else {
      out <- NA
    }
    Sys.sleep(0.2)   
    
    return(out)
  }
  
  
  #Set up the queries
  
  query <- unlist(strsplit(query,","))
  
  
  
  number.items.per.batch<-1
  number.of.batches<-ceil(length(query)/number.items.per.batch)
  
  tweets<-data.frame(text=character(), id=character(), screenName=character(), token=character(), stringsAsFactors = F)
  
  
  
  
  
  
  
  y=1
  geo=geocodeAdddress(loc, radius)
  
  for(z in 1:number.of.batches)
  {
    print (paste("Working on batch", z, "out of ", number.of.batches))
    start<-((z-1)*number.items.per.batch)+1
    stop<-ifelse(z*number.items.per.batch > length(query), length(query), z*number.items.per.batch)
    
    terms<-paste('"', query[start:stop], '"',  sep="", collapse=" OR ")
    terms<-paste("'", terms, "'", sep="")
    tryCatch({
      
      suppressWarnings( result<-searchTwitter(terms,n=ntweets, geocode = geo, resultType = "recent"))
      result<-strip_retweets(result)
      
      
      if(length(result)>0)
      {
        
        
        df <- do.call("rbind", lapply(result, as.data.frame))
        apidata<-data.frame(text=character(), id=character(), screenName=character(), token=character(), stringsAsFactors = F)
        
        if (!is.null(df))
        {
          
          print("api: success")
          
          
          apidata[1:nrow(df), "token"]<-"query.term"
          apidata$id<-df$id
          apidata$screenName<-df$screenName
          apidata$text<-df$text
          apidata<-apidata[!duplicated(apidata$id), ]
          if (!is.null(excludedids))
          {
            apidata<-apidata[apidata$id %nin% excludedids, ] 
          }
          
          #parse tweets
          
          
          #merge all tweets to one string, pass it to the parser, and split it up again afterwards (saves time)
          
          
          rbind(apidata, tweets)->tweets
          tweets<- tweets[!duplicated(tweets$id), ]
          if (!is.null(excludedids))
          {
            tweets<-tweets[tweets$id %nin% excludedids, ] 
          }
          
          print(paste( nrow(tweets), " tweets"))
        }
        
        
      }
    }, warning=function(w){cat(as.character(w), "\n", file="warnings.txt", append=T)},
    error=function(e){print(e)},
    finally={
      
      
      if(length(result)>0)
      {
        
        
        df <- do.call("rbind", lapply(result, as.data.frame))
        apidata<-data.frame(text=character(), id=character(), screenName=character(), token=character(), stringsAsFactors = F)
        
        if (!is.null(df))
        {
          
          print("api: success")
          
          
          apidata[1:nrow(df), "token"]<-"query.term"
          apidata$id<-df$id
          apidata$screenName<-df$screenName
          apidata$text<-df$text
          apidata<-apidata[!duplicated(apidata$id), ]
          
          
          
          
          
          rbind(apidata, tweets)->tweets
          tweets<- tweets[!duplicated(tweets$id), ]
          
          print(paste( nrow(tweets), " tweets"))
          
          
          
          
        }
      }
    })
    save(tweets, file="tweets.rdata")
    
    
    
  }
  
  #remove the placeholders
  tweets$token<-NULL
  #Merge the data in the file with our new tweets
  
  
  # Remove duplicates and retweets
  tweetsoriginal<-tweets
  tweets<-tweets[!duplicated(tweets$id), ]
  tweets<-tweets[!duplicated(tweets$text), ]
  tweets$text<-stri_replace_all_fixed(tweets$text, "  ", " ")
  tweets$text<-stri_replace_all_fixed(tweets$text,";", "")
  tweets$text<-stri_replace_all_fixed(tweets$text,":", "")
  tweets$text<-stri_replace_all_fixed(tweets$text,"\"", "")
  tweets$text<-stri_replace_all_fixed(tweets$text,"\n", "")
  tweets$text<-stri_replace_all_fixed(tweets$text,"\r", "")
  tweets$text<-stri_replace_all_fixed(tweets$text,"[^[:alnum:] -,:._']", "")
  tweets<-tweets[!duplicated(stri_trans_tolower(tweets$text)), ]
  tweets$text <- stri_replace_all_regex(tweets$text, "[^[:print:]]", "")
  tweets$text <- stri_replace_all_regex(tweets$text, "[^[:alnum:] -,._'@]", "")  
  
  
  #Concatenate, parse, and split
  
  
  print("Parsing…")
  
  #The parser will freeze if the texts are too large (it will run out of memory, even we allocate 6GB for it). We push batches of 5K tweets at a time.
  
  number<-nrow(tweets)
  batches<-ceil(number/5000)
  parsed<-c()
  for (x in 1:batches)
  {
    start<-((x-1)*5000)+1
    stop<-ifelse(x*5000 > number, number, x*5000)
    
    texts <- paste(tweets[start:stop, "text"], collapse = "\n")
    cat(trimws(stri_trans_tolower(texts)), file="temp.txt")
    
    system(paste0("java -mx6144m -cp ", parserPath, " edu.stanford.nlp.tagger.maxent.MaxentTagger -model ", parserModel," -textFile ", getwd(),"/temp.txt -outputFormat slashTags > ",getwd(),"/parsed.txt -sentenceDelimiter newline",  collapse=""))
    output<-readLines("parsed.txt")
    parsed<-c(parsed, output)
  }
  

  tweets$parsed.text<-parsed
  
  
  #end parsing code
  
  #Save
  
  
  
  
  
  return(tweets)
}