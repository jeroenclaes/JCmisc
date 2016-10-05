#READ ME CAREFULLY!

#This script is released "as is" under the terms of the Creative Commons #Attribution-Non Commercial 2.0 Generic (CC BY-NC 2.0) license, without any #warranty. You may use them at your own risk, provided you respect the terms #of the license.
#https://creativecommons.org/licenses/by-nc/2.0/

#This script will read text files, parse them with the Stanford POS tagger, and extract/annotate all conjugated verbs. 
#It was designed for use in an investigation on variable absence/presence of Spanish subject personal pronouns
#The script divides sentence units into clauses (conjugated verbs) and annotates these clause units for the following features: verb lemma, person and number, tense, mode, absence/presence of negation, clause number, number of words between verb and pronoun site, words between verb and pronoun site, and tense aspect. 
#***Needless to say, the output requires careful manual post-editing.****

#To use this script:
#1. Download the **FULL** Stanford tagger from the Stanford NLP Group website (http://nlp.stanford.edu/software/stanford-postagger-full-2015-04-20.zip). You may also want to update the JAVA installation on your computer
#2. Unzip the Stanford tagger
#3. Remember where you unzipped the tagger :). 
#. Run findVerbs(file.choose(), "path to your installation of the parser", "path to the installation of the parserModel")
#4.Find file.csv. This script uses ";" as field separator, if you're in a 'comma' csv region, do a search/replace to change 'write.csv2' to 'write.csv'

findVerbs<-function(file, parserPath, parserModel)
{
  
 
require("XML")
require("stringr")
require("foreach")
   z=1
  verbs=1
start.time<-proc.time()
corpus.df<<-data.frame(sentence.id=numeric(), verb.nr=numeric(), who=character(),before=character(), token=character(),after=character(),verb.lemma=character(), verb.person=character(), type=character(), tense=character(),    number.words.before.verb=numeric(), words.before.verb=character(), negation=character(), mode=character(), aspect=character(),  stringsAsFactors=FALSE)
discarded.df<<-data.frame(sentence=character(), sentence.id=numeric(), stringsAsFactors = FALSE)

system(paste0("java -cp ", parserPath, " edu.stanford.nlp.tagger.maxent.MaxentTagger -model ", parserModel," -textFile ", file," -outputFormat xml > ",getwd(),"/document.xml", sep="", collapse="")) 

xmltop<-xmlRoot(xmlParse("document.xml"))
file.remove("document.xml")
total<-xmlSize(xmltop)

#loop through sentences
foreach (i=1:total) %do%
{
  tryCatch({
  vector.v<-c()
#loop through words in sentences  
  foreach (y= 1:xmlSize(xmltop[[i]])) %do%
  {
  
if(grepl("vs|vmi|va|vms|dp|nc|da|pp|np|p0", xmlAttrs(xmltop[[i]][[y]])["pos"]))
    {
      
      v<-paste0(c(xmlValue(xmltop[[i]][[y]]), xmlAttrs(xmltop[[i]][[y]])["pos"]), collapse="/")
    }
    else
    {
      v<-xmlValue(xmltop[[i]][[y]])
    }
  append(vector.v, v)->vector.v
  }

 sentence<-trimws(paste(vector.v, collapse=" "))
 if(grepl("RRB|LRB", sentence))
 {
   if(grepl("Aja.", sentence))
   {
     who<-"speaker"   
   }
   else
   {
     who<-"interviewer"   
   }
  
   
 }
 else
 {
   who<-"speaker"
 }
 
 sentence<-gsub("-RRB-/vmis000", ")", sentence)
 sentence<-gsub("/vmn0000", "", sentence)
 sentence<-gsub("/vmp0000", "", sentence)
 sentence<-gsub("/vap0000", "", sentence)
 sentence<-gsub("o sea/vssp000", "o sea", sentence)
 sentence<-gsub("O sea/vssp000", "O sea", sentence)
 sentence<-gsub("/vsn0000", "", sentence)
 sentence<-gsub("/vsp0000", "", sentence)
 sentence<-gsub("es/vsip000 decir", "es decir", sentence)
 sentence<-gsub("Es/vsip000 decir", "es decir", sentence)
 sentence<-gsub(" -LRB-/np00000", "(", sentence)
 sentence<-gsub("-RRB-/np00000", ")", sentence)
 sentence<-gsub("-LRB-", "(", sentence)
 sentence<-gsub("-RRB-", ")", sentence)
 sentence<-gsub("( Risas/np00000 )/np00000","", sentence)
 sentence<-gsub("( Aja/np00000 . )", "", sentence)
 sentence<-gsub("Aja/np00000 .", "", sentence)
 sentence<-gsub("()", "", sentence)
 sentence<-gsub("acá/vmn0000", "", sentence)
 sentence<-gsub("Acá/vmn0000", "", sentence)
 
 count.v<-as.integer(str_count(sentence, "([a-zA-Z\\xE1\\xE9\\xED\\xF3\\xFA\\xC1\\xC9\\xCD\\xD3\\xDA\\xF1\\xD1]+[/][v][a-z]+[0]+)\\w")[[1]])
 if (count.v > 1)
 {
  instances<- str_locate_all(sentence, "\\b([a-zA-Z\\xE1\\xE9\\xED\\xF3\\xFA\\xC1\\xC9\\xCD\\xD3\\xDA\\xF1\\xD1]+[/][v][a-z]+[0]+)\\b")
  total<-as.integer(str_count(sentence, "\\b([a-zA-Z\\xE1\\xE9\\xED\\xF3\\xFA\\xC1\\xC9\\xCD\\xD3\\xDA\\xF1\\xD1]+[/][v][a-z]+[0]+)\\b")[[1]])
   foreach (x=1:total) %do%
   {
     start<-instances[[1]][x, "start"]
       stop<-instances[[1]][x, "end"]
     token<-trimws(substring(sentence,start, stop))
     verbs+1->verbs
   if(x==1)
   {
     clause<-trimws(substring(sentence, 0, instances[[1]][x+1, "start"]))
   }
     else if(x==total)
     {
       clause<-trimws(substring(sentence, instances[[1]][x-1, "end"]+1))

     }
     else
     {
       clause<-trimws(substring(sentence, instances[[1]][x-1, "end"], instances[[1]][x+1, "start"]))   
     }
     
     if (grepl("\\b([a-zA-Z\\xE1\\xE9\\xED\\xF3\\xFA\\xC1\\xC9\\xCD\\xD3\\xDA\\xF1\\xD1]+[/][v][a-z]+[0]+)\\b[\\s]\\b([a-z]+(ado|ido))\\b", clause, perl=T))
     {
       token<-trimws(str_extract(clause, "\\b([a-zA-Z\\xE1\\xE9\\xED\\xF3\\xFA\\xC1\\xC9\\xCD\\xD3\\xDA\\xF1\\xD1]+[/][v][a-z]+[0]+)\\b[\\s]\\b([a-z]+(ado|ido))\\b"))
     }
     else
     {
       token<-trimws(str_extract(clause, "\\b([a-zA-Z\\xE1\\xE9\\xED\\xF3\\xFA\\xC1\\xC9\\xCD\\xD3\\xDA\\xF1\\xD1]+[/][v][a-z]+[0]+)\\b"))
     }

     if (grepl("((yo|tú|él|ella|usted|nosotros|vosotros|ustedes|ellos|ellas|uno)[/][a-z]+[0]+)\\b(([\\s](\\bno\\b|\\bnunca\\b)[\\s]\\b([a-z]+[/][a-z]+[0]+)\\b[\\s]\\b([a-z]+[/][a-z]+[0]+)\\b[\\s])|([\\s]\\b([a-z]+[/][a-z]+[0]+)\\b[\\s]\\b([a-z]+[/][a-z]+[0]+)\\b[\\s])|([\\s]\\b(no|nunca)\\b[\\s]([a-z]+[/][a-z]+[0]+)\\b[\\s])|([\\s]\\b(no|nunca)\\b[\\s])|([\\s]\\b[a-z]+[/][a-z]+[0]+)\\b[\\s]|[\\s])([a-zA-Z\\xE1\\xE9\\xED\\xF3\\xFA\\xC1\\xC9\\xCD\\xD3\\xDA\\xF1\\xD1]+[/][v][a-z]+[0]+)\\b", tolower(clause), perl=T))
     {
       type<-"preposed.pronoun"
     }
     else if (grepl("([a-zA-Z\\xE1\\xE9\\xED\\xF3\\xFA\\xC1\\xC9\\xCD\\xD3\\xDA\\xF1\\xD1]+[/][v][a-z]+[0]+)\\b[\\s](yo|tú|él|ella|usted|nosotros|vosotros|ustedes|ellos|ellas|uno)\\b", tolower(clause), perl=T))
     {
       type<-"postposed.pronoun"  
     }
     else
     {
       type<-"zero"
     }
     
     tense<-trimws(gsub("[0]+","",  str_extract(token, "([/][v][a-z]+[0]+)\\w"), perl=T))
     tense<-gsub("[/]", "", tense, perl=T)
     
     #human-readable tense labels & code for aspect
     tense<-gsub("vmip", "present", tense)
     tense<-gsub("vmii", "imperfect", tense)
     tense<-gsub("vsip", "present", tense)
     tense<-gsub("vsii", "imperfect", tense)
     tense<-gsub("vmsp", "present.subjunctive", tense)
     tense<-gsub("vssp", "present.subjunctive", tense)
     tense<-gsub("vmic", "conditional", tense)
     tense<-gsub("vsic", "conditional", tense)
     tense<-gsub("vmis", "preterit", tense)
     tense<-gsub("vsis", "preterit", tense)
     tense<-gsub("vmsi", "past.subjunctive", tense)
     tense<-gsub("vssi", "past.subjunctive", tense)
     tense<-gsub("vsif", "morphological.future", tense)
     tense<-gsub("vmif", "morphological.future", tense)
     if ((str_count(token, " ") > 0) && (tense=="vaip"))
     {
       tense<-gsub("vaip", "perfect", tense)
     }
     else if ((str_count(token, " ") > 0) && (tense=="vaii"))
     {
       tense<-gsub("vaip", "pluperfect", tense)
     }
     else if ((str_count(token, " ") > 0) && (tense=="vaif"))
     {
       tense<-gsub("vaip", "future.perfect", tense)
       aspect<-"perfective"
     }
     else if((any(grep("\\b([a-z]+[/][va][a-z]+[0]+)\\b[\\s]\\b[a-z]+", sentence, perl=T))) && (any(grep("va", tense))))
     {
       tense<-"existential.haber?"
       aspect<-"NA"
       
     }
     if (any(grep("morphological.future|past.subjunctive|conditional|present.subjunctive|imperfect|present", tense)))
     {
       aspect<-"imperfective"
     }
     else if (any(grep("preterit|perfect", tense)))
     {
       aspect<-"perfective"
     }
     else
     {
       aspect<-"NA"
     }
   
#extract verb domain, negation/pronouns before verb
     verb.domain<- trimws(str_extract(gsub("((yo|tú|él|ella|usted|nosotros|vosotros|ustedes|ellos|ellas|uno)[/][a-u]+[0]+)\\w", "", tolower(clause), perl=T), "(((\\bno\\b|\\bnunca\\b)[\\s]\\b([a-z]+[/][a-u]+[0]+)\\b[\\s]\\b([a-z]+[/][a-u]+[0]+)\\b)|(\\b([a-z]+[/][a-u]+[0]+)\\b[\\s]\\b([a-z]+[/][a-u]+[0]+)\\b)|((\\bno\\b|\\bnunca\\b)[\\s]([a-z]+[/][a-u]+[0]+)\\b)|((\\bno\\b|\\bnunca\\b))|(\\b[a-z]+[/][a-u]+[0]+))\\b[\\s]([a-zA-Z\\xE1\\xE9\\xED\\xF3\\xFA\\xC1\\xC9\\xCD\\xD3\\xDA\\xF1\\xD1]+[/][v][a-z]+[0]+)\\b"))
     verb.domain.no.verb<-trimws(str_extract(tolower(verb.domain), "(((\\bno\\b|\\bnunca\\b)[\\s]\\b([a-z]+[/][a-u]+[0]+)\\b[\\s]\\b([a-z]+[/][a-u]+[0]+)\\b)|(\\b([a-z]+[/][a-u]+[0]+)\\b[\\s]\\b([a-z]+[/][a-u]+[0]+)\\b)|((\\bno\\b|\\bnunca\\b)[\\s]([a-z]+[/][a-u]+[0]+)\\b)|((\\bno\\b|\\bnunca\\b))|(\\b[a-z]+[/][a-u]+[0]+))\\b"))
words<-str_count(trimws(tolower(verb.domain.no.verb)), "\\b[a-z]+\\b")
#coding for negation
negation<-grepl("(\\bno\\b|\\bnunca\\b)", tolower(verb.domain), perl=T)
if(isTRUE(negation)) 
{
  negation<-"present"

}
else
{
  negation<-"absent"
}


if((negation=="present") || (tense=="present.subjunctive")||(tense=="morphological.future")||(tense=="past.subjunctive")||(tense=="conditional"))
{
  mode<-"irrealis"
}
else
{
  mode<-"realis"
}
verb.domain.no.verb<-trimws(gsub("([/][a-z]+[0]+)", "", verb.domain.no.verb, perl=T))
token<-trimws(gsub("([/][a-z]+[0]+)", "", token, perl=T))
token.original<-token
token<-tolower(token)
 
#clean up text for readability
clause <-trimws(gsub("([/][a-z]+[0][a-z]+[0]+)", "",clause, perl=T))
clause <-trimws(gsub("([/][a-z]+[0]+)", "", clause, perl=T))   
#split up clause
split<-strsplit(clause, token)
before<-trimws(split[[1]][1])
after<-trimws(split[[1]][2])
before<-gsub("()", "", before)
after<-gsub("()", "", after)

#lookup verb and annotate
verb.info<-lookup.verb(token)
verb.lemma<-verb.info[[1]]
verb.person<-verb.info[[2]]



#writing to dataframe
newrow.corpus<-data.frame(sentence.id=xmlAttrs(xmltop[[i]]),verb.nr=verbs,who=who, before=before,  token=token, after=after, verb.lemma=verb.lemma, verb.person=verb.person, type=type, tense=tense,   number.words.before.verb=words, words.before.verb=verb.domain.no.verb,negation=negation, mode=mode, aspect=aspect, stringsAsFactors=FALSE)
     rbind(globalenv()$corpus.df, newrow.corpus)->>corpus.df
     
   }
   
 }
 else if(count.v==1)
 {
   verbs+1->verbs

   
   if (grepl("([a-zA-Z\\xE1\\xE9\\xED\\xF3\\xFA\\xC1\\xC9\\xCD\\xD3\\xDA\\xF1\\xD1]+[/][v][a-z]+[0]+)\\w[\\s]([a-z]+(ado|ido))", tolower(sentence), perl=T))
   {
     token<-str_extract(tolower(sentence), "([a-zA-Z\\xE1\\xE9\\xED\\xF3\\xFA\\xC1\\xC9\\xCD\\xD3\\xDA\\xF1\\xD1]+[/][v][a-z]+[0]+)\\w[\\s]([a-z]+(ado|ido))") 
   }
   else
   {
     token<-trimws(str_extract(sentence, "([a-zA-Z\\xE1\\xE9\\xED\\xF3\\xFA\\xC1\\xC9\\xCD\\xD3\\xDA\\xF1\\xD1]+[/][v][a-z]+[0]+)\\w"))
   }
   
   if (grepl("((yo|tú|él|ella|usted|nosotros|vosotros|ustedes|ellos|ellas|uno)[/][a-z]+[0]+)\\b(([\\s](\\bno\\b|\\bnunca\\b)[\\s]\\b([a-z]+[/][a-z]+[0]+)\\b[\\s]\\b([a-z]+[/][a-z]+[0]+)\\b[\\s])|([\\s]\\b([a-z]+[/][a-z]+[0]+)\\b[\\s]\\b([a-z]+[/][a-z]+[0]+)\\b[\\s])|([\\s](\\bno\\b|\\bnunca\\b)[\\s]([a-z]+[/][a-z]+[0]+)\\b[\\s])|([\\s](\\bno\\b|\\bnunca\\b)[\\s])|([\\s]\\b[a-z]+[/][a-z]+[0]+)\\b[\\s]|[\\s])([a-zA-Z\\xE1\\xE9\\xED\\xF3\\xFA\\xC1\\xC9\\xCD\\xD3\\xDA\\xF1\\xD1]+[/][v][a-z]+[0]+)\\b", tolower(sentence), perl=T))
       {
type<-"preposed.pronoun"
   }
   else if (grepl("([a-zA-Z\\xE1\\xE9\\xED\\xF3\\xFA\\xC1\\xC9\\xCD\\xD3\\xDA\\xF1\\xD1]+[/][v][a-z]+[0]+)\\w[\\s](yo|tú|él|ella|usted|nosotros|vosotros|ustedes|ellos|ellas|uno)\\w", tolower(sentence), perl=T))
   {
     type<-"postposed.pronoun"  
   }
   else
   {
     type<-"zero"
   }
   
   
   #human-readable tense labels and code for aspect

   tense<-trimws(gsub("[0]+","",  str_extract(token, "([/][v][a-z]+[0]+)\\w"), perl=T))
   tense<-gsub("[/]", "", tense, perl=T)
   
   tense<-gsub("vmip", "present", tense)
   tense<-gsub("vmii", "imperfect", tense)
   tense<-gsub("vsip", "present", tense)
   tense<-gsub("vsii", "imperfect", tense)
   tense<-gsub("vmsp", "present.subjunctive", tense)
   tense<-gsub("vssp", "present.subjunctive", tense)
   tense<-gsub("vmic", "conditional", tense)
   tense<-gsub("vsic", "conditional", tense)
   tense<-gsub("vmis", "preterit", tense)
   tense<-gsub("vsis", "preterit", tense)
   tense<-gsub("vmsi", "past.subjunctive", tense)
   tense<-gsub("vssi", "past.subjunctive", tense)
   tense<-gsub("vsif", "morphological.future", tense)
   tense<-gsub("vmif", "morphological.future", tense)
   if ((str_count(token, " ") > 0) && (tense=="vaip"))
   {
     tense<-gsub("vaip", "perfect", tense)
   }
   else if ((str_count(token, " ") > 0) && (tense=="vaii"))
   {
     tense<-gsub("vaip", "pluperfect", tense)
   }
   else if ((str_count(token, " ") > 0) && (tense=="vaif"))
   {
     tense<-gsub("vaip", "future.perfect", tense)
     aspect<-"perfective"
   }
   else if((any(grep("\\b([a-z]+[/][va][a-z]+[0]+)\\b[\\s]\\b[a-z]+", sentence, perl=T))) && (any(grep("va", tense))))
   {
     tense<-"existential.haber?"
     aspect<-"NA"
     
   }
   if (any(grep("morphological.future|past.subjunctive|conditional|present.subjunctive|imperfect|present", tense)))
   {
     aspect<-"imperfective"
   }
   else if (any(grep("preterit|perfect", tense)))
   {
     aspect<-"perfective"
   }
   else
   {
     aspect<-"NA"
   }
   
   verb.domain<- trimws(str_extract(gsub("((yo|tú|él|ella|usted|nosotros|vosotros|ustedes|ellos|ellas|uno)[/][a-u]+[0]+)\\w", "", tolower(sentence), perl=T), "(((\\bno\\b|\\bnunca\\b)[\\s]\\b([a-z]+[/][a-u]+[0]+)\\b[\\s]\\b([a-z]+[/][a-u]+[0]+)\\b)|(\\b([a-z]+[/][a-u]+[0]+)\\b[\\s]\\b([a-z]+[/][a-u]+[0]+)\\b)|((\\bno\\b|\\bnunca\\b)[\\s]([a-z]+[/][a-u]+[0]+)\\b)|((\\bno\\b|\\bnunca\\b))|(\\b[a-z]+[/][a-u]+[0]+))\\b[\\s]([a-zA-Z\\xE1\\xE9\\xED\\xF3\\xFA\\xC1\\xC9\\xCD\\xD3\\xDA\\xF1\\xD1]+[/][v][a-z]+[0]+)\\b"))
   verb.domain.no.verb<-trimws(str_extract(tolower(verb.domain), "(((\\bno\\b|\\bnunca\\b)[\\s]\\b([a-z]+[/][a-u]+[0]+)\\b[\\s]\\b([a-z]+[/][a-u]+[0]+)\\b)|(\\b([a-z]+[/][a-u]+[0]+)\\b[\\s]\\b([a-z]+[/][a-u]+[0]+)\\b)|((\\bno\\b|\\bnunca\\b)[\\s]([a-z]+[/][a-u]+[0]+)\\b)|((\\bno\\b|\\bnunca\\b))|(\\b[a-z]+[/][a-u]+[0]+))\\b"))
  
    words<-str_count(trimws(tolower(verb.domain.no.verb)), "\\b[a-z]+\\b")
   negation<-grepl("(no|nunca)", tolower(verb.domain), perl=T)
   negation<-grepl("(\\bno\\b|\\bnunca\\b)", tolower(verb.domain), perl=T)
   if(isTRUE(negation)) 
   {
     negation<-"present"
     
   }
   else
   {
     negation<-"absent"
   }
   
   
   if((negation=="present") || (tense=="present.subjunctive")||(tense=="morphological.future")||(tense=="past.subjunctive")||(tense=="conditional"))
   {
     mode<-"irrealis"
   }
   else
   {
     mode<-"realis"
   }
   
 
   #clean up text for readability
   sentence <-trimws(gsub("([/][a-z]+[0][a-z]+[0]+)", "",sentence, perl=T))
   sentence <-trimws(gsub("([/][a-z]+[0]+)", "", sentence, perl=T))   
   
   verb.domain.no.verb<-trimws(gsub("([/][a-z]+[0]+)", "", verb.domain.no.verb, perl=T))
   token<-trimws(gsub("([/][a-z]+[0]+)", "", token, perl=T))
   
   token.original<-token
   token<-tolower(token)
   
   #split up clause in before - after token
   split<-strsplit(sentence, token.original)
   before<-trimws(split[[1]][1])
   after<-trimws(split[[1]][2])
   before<-gsub("()", "", before)
   after<-gsub("()", "", after)
   #lookup verb person/number and infinitive and annotate
   verb.info<-lookup.verb(token)
  verb.lemma<-verb.info[[1]]
   verb.person<-verb.info[[2]]
  
   
   newrow.corpus<-data.frame(sentence.id=xmlAttrs(xmltop[[i]]),verb.nr=verbs,who=who, before=before, token=token, after=after, verb.lemma=verb.lemma, verb.person=verb.person, type=type, tense=tense,   number.words.before.verb=words, words.before.verb=verb.domain.no.verb,negation=negation, mode=mode, aspect=aspect, stringsAsFactors=FALSE)
   
   rbind(globalenv()$corpus.df, newrow.corpus)->>corpus.df
 }
 else
 {
 if(grepl("dp|nc|da|pp|np", sentence))
 {
   #clean up text for readability
   token<-"NA"
   tense<-"NA"
   type<-"NPs-nouns-dps"
   negation<-"NA"
   words<-0
   verb.domain.no.verb<-"NA"
   mode<-"NA"
   verb.lemma<-"NA"
   verb.person<-"NA"
   sentence <-gsub("([/][a-u]+[0][a-z]+[0]+)", "", sentence, perl=T)
   sentence <-gsub("([/][a-u]+[0]+)", "", sentence, perl=T)
   before<-sentence
   after<-"NA"
   aspect<-"NA"

 

   newrow.corpus<-data.frame(sentence.id=xmlAttrs(xmltop[[i]]),verb.nr=verbs,who=who, before=before, token=token, after=after, verb.lemma=verb.lemma,verb.person=verb.person, type=type, tense=tense,   number.words.before.verb=words, words.before.verb=verb.domain.no.verb,negation=negation, mode=mode, aspect=aspect, stringsAsFactors=FALSE)
   
   
   rbind(globalenv()$corpus.df, newrow.corpus)->>corpus.df
 }
   else
   {
     
     newrow.discarded<-data.frame(sentence=sentence, sentence.id=xmlAttrs(xmltop[[i]]))
     rbind(globalenv()$discarded.df, newrow.discarded)->>discarded.df
   }
  
       
       z+1->z
 }
  }, 
warning=function(w){print(as.character(w))},
error=function(e){print(as.character(e))} )
}
elapsed<-proc.time()-start.time

cat ("\n Searching the data took ", elapsed[[3]], " seconds \n")
cat("\n Skipped", z, "sentence units which did not contain content words\n")
cat("\n Call the variable 'discarded.df' to inspect them.\n")
write.csv2(corpus.df, file="file.csv", fileEncoding="iso-8859-1")

}  

lookup.verb<-function(token)
{
  token<<-token
database<-read.csv2("/Users/jeroenclaes/Dropbox/Public/Google Drive/PostDoc/Methods/verbInfo.csv", header=T, row.names=1, fileEncoding="iso-8859-1", stringsAsFactors=F)
lemma.result<-"NA"
conjugation<-"NA"

#let's see if we have the token already
tryCatch({
if(any(which(database[,"token"]==as.character(trimws(token))))) 
{
lemma.result<-as.character(database[which(database[,"token"]==as.character(trimws(token)))[1], "verb.lemma"])
conjugation<-as.character(database[which(database[,"token"]==as.character(trimws(token)))[1], "verb.person"])
  
}
else
{

require("RCurl")
  require("stringr")

url<- paste(
  "http://cartago.lllf.uam.es/grampal/grampal.cgi?m=etiqueta&e=",
  gsub(" ", "%20", trimws(token)), sep = "" )

page<- getURL(url = url,header = F, .encoding="UTF-8")
conjugation<-str_extract_all(page, "(singular|plural)&nbsp;&nbsp;[1-6]", T)
lemma<-str_extract_all(page, "lema&nbsp;&nbsp;&nbsp;<b>[a-zA-Z\\xE1\\xE9\\xED\\xF3\\xFA\\xC1\\xC9\\xCD\\xD3\\xDA\\xF1\\xD1]+</b>", T)


category<-str_extract(page, "categoría&nbsp;&nbsp;&nbsp;<b>[A-Z]+</b>")
category<-sub("categoría&nbsp;&nbsp;&nbsp;<b>", "", category)
category<-sub("</b>", "", trimws(category))

if(any(grep("(lema&nbsp;&nbsp;&nbsp;<b>UNKN</b>)|(rasgos&nbsp;&nbsp;&nbsp;<b>&nbsp;gerundio&nbsp;</b>)|(rasgos&nbsp;&nbsp;&nbsp;<b>&nbsp;infinitivo&nbsp;</b>)", page, perl=T)))
{
  conjugation<-"NA"
  lemma.result<-"NA"
}
else if ((any(grep("[A-U]+", category, perl=T))&&(!any(grep("AUX", category, perl=T)))))
{
  conjugation<-"NA"
  lemma.result<-"NA" 
}
else
{
  conjugation<-gsub("&nbsp;", " ", conjugation[1,1])
  conjugation<-trimws(sub("  ", " ", conjugation))
  conjugation<-gsub(" ", ".", conjugation)
if(ncol(lemma)>1)
{
  lemma.result<-gsub("lema&nbsp;&nbsp;&nbsp;<b>", "", lemma[1,2])
  lemma.result<-sub("</b>", "", lemma.result)
  lemma.result<-trimws(tolower(lemma.result))
}
else
{
  lemma.result<-gsub("lema&nbsp;&nbsp;&nbsp;<b>", "", lemma[1,1])
  lemma.result<-sub("</b>", "", lemma.result)
  lemma.result<-trimws(tolower(lemma.result))
}
  update<-data.frame(token=as.character(token), verb.lemma=as.character(lemma.result), verb.person=as.character(conjugation), stringsAsFactors = F)
  rbind(database,update)->database
  write.csv2(database, "/Users/jeroenclaes/Dropbox/Public/Google Drive/PostDoc/Methods/verbInfo.csv",fileEncoding ="iso-8859-1")
}

}

  result<-list(lemma=lemma.result, conjugation=conjugation)
  return(result)
  
},warning=function(w) {
  cat("Warning: ", str_trim(as.character(w)), "for token ", globalenv()$token) 
  cat ("\n <NA> returned\n.")
  result<-list(lemma="NA", conjugation="NA")
  return(result)
},  
error= function(e) {
  cat("Error: ", str_trim(as.character(e)), "for token ", globalenv()$token); 
  cat ("\n <NA> returned\n.")
  result<-list(lemma="NA", conjugation="NA")
  return(result)
})

}
