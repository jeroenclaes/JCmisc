wordnetAnimacyES<-function(wordVector)
{
  require(RCurl)
  require(XML)
  require(stringi)
  
result<-data.frame(words=wordVector, domain="empty")

result$domain<-sapply(result$words, FUN=function(x) {
  
  page<-getURL(paste0("http://adimen.si.ehu.es/cgi-bin/wei/public/wei.consult.perl?item=",x,"&button1=Look_up&metode=Word&llengua=Spanish_3.0&search=near_synonym&estructura=English_3.0&glos=Gloss&csco=Score&rels=Rels&full=Full&levin=1&spa-30=Spanish_3.0", collapse=""))
  e<-stri_extract_first_regex(page, "<script>[\n[:print:]]+</script>")
  f<-stri_split_fixed(e, ";")
  z<-stri_split_fixed(f[[1]][3], "=")[[1]][2]
  z<-trimws(stri_replace_all_fixed(z, '\"', ''))
  
return(z)
})
  return(result)
}