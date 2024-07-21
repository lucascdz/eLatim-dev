# functions to create JSON-LD entries

standardName <- c('headword','PoS' ,'equivalent','definition','example','equivalent_example','multi-word expression', "variant","absolute_frequency_NUMERIC")
lexicogName <- c('ontolex:LexicalEntry','lexinfo:partOfSpeech' ,'vartrans:Translation', 'ontolex:LexicalConcept', 'lexicog:UsageExample','lexicog:UsageExample',"ontolex:MultiwordExpression","ontolex:writtenRep","frac:Frequency")
lexicogNamesDF <- data.frame(standardName, lexicogName, stringsAsFactors = F)
write.csv(lexicogNamesDF, "./lexicogNamesDF.csv",row.names = F)

# create content for LexicographicResource:

GetDictCreds <- function(){
  if(file.exists("./MyDict/DictCredits.txt")){
    DictCredits <- readtext("./MyDict/DictCredits.txt")$text
    dictTitle <- str_extract(DictCredits, "title=.*?author") %>% str_remove_all(.,"title=|author")
    dictAuthor <- str_extract(DictCredits, "author=.*?$") %>% str_remove(.,"author=")
  }else if(file.exists("./MyDict/_quarto.yml")){
    yamlFile <- readtext::readtext("./MyDict/_quarto.yml")$text
    dictTitle <- str_extract(yamlFile, 'title: ".*?"') %>% str_remove_all(.,'title: |"')
    dictAuthor <- str_extract(yamlFile, 'author: ".*?"') %>% str_remove_all(.,'author: |"')
  }else{
    dictTitle <- "title not available"
    dictAuthor <- "unknown"
  }

  DICT_TITLE <- paste0(dictAuthor, ". ", dictTitle)
  return(DICT_TITLE)
}
# extract relevant @context

NameSpacesDF <- data.frame(prefix=c("ontolex","lexicog","vartrans","lexinfo","frac"),
                           nameSpace=c("http://www.w3.org/ns/lemon/ontolex#",
                                       "http://www.w3.org/ns/lemon/lexicog#",
                                       "http://www.w3.org/ns/lemon/vartrans#",
                                       "http://www.lexinfo.net/ontology/2.0/lexinfo#",
                                       "http://www.w3.org/nl/lemon/frac#"), stringsAsFactors = F)



GetContextAttr <- function(lexicog_tag, NameSpacesDF){

  splitTag <- unlist(str_split(lexicog_tag,":"))
  prefix <-  splitTag[1]
  name <-  splitTag[2]

  contextString <- paste0('"',name,'": ', '"',NameSpacesDF$nameSpace[NameSpacesDF$prefix==prefix],name, '",')

}

CreateContextElement <- function(DFlexicogName=DF$lexicogName, NameSpacesDF){
  context <- unlist(lapply(DFlexicogName, function(x) GetContextAttr(x, NameSpacesDF)))
  context <- paste(context, collapse = "\n")
  context <- paste('"@context":{','"LexicographicResource":"http://www.w3.org/ns/lemon/lexicog#LexicographicResource",', context,'  },', sep="\n" )
  return(context)
}

GetSharableEntryElement <- function(lexicogNameVal, Entry, DF){

  if(length(which(DF$lexicogName==lexicogNameVal))==1){
    # unnest deeply nested entry elements

    newElement <- Entry[[1]][[DF$elementName[DF$lexicogName==lexicogNameVal]]][[1]]

  }else if(length(which(DF$lexicogName==lexicogNameVal))>1){
    # split by language any element repeated but with different language

    ByLang <- lapply(DF$lang[DF$lexicogName==lexicogNameVal], function(x) Entry[[1]][[DF$elementName[DF$lexicogName==lexicogNameVal & DF$lang==x]]][[1]] )
    names(ByLang) <- DF$lang[DF$lexicogName==lexicogNameVal]
    newElement <- list(ByLang)
  }

  if(lexicogNameVal=="vartrans:Translation"){
    # entry has a translation element, split it by language putting the headword here as well for bid-derectionality
    TranslEl <- list(newElement, Entry[[1]][[DF$elementName[DF$lexicogName=="ontolex:LexicalEntry"]]][[1]])
    if(is.null(names(newElement))){
      names(TranslEl) <- c(DF$lang[DF$lexicogName==lexicogNameVal], DF$lang[DF$lexicogName=="ontolex:LexicalEntry"])
    }else{
      names(TranslEl)[length(names(TranslEl))] <- DF$lang[DF$lexicogName=="ontolex:LexicalEntry"]
    }
    newElement <- TranslEl
   # names(newElement) <- "Translation"
  }

  return(newElement)
}

MakeSharableEntry <- function(Entry,DF, credits,context){
  # DF is the df created in this app, it needs to have cols lexicogName, lang & elementName
  #e.g.:
  #           elementName       standardName lang          lexicogName
  # 1             english           headword   en ontolex:LexicalEntry
  # 2    english examples            example   en lexicog:UsageExample
  # 3          portuguese         equivalent   pt vartrans:Translation
  # 4 portuguese examples equivalent_example   pt lexicog:UsageExample

  if(length(which(duplicated(DF$lexicogName)))>0){
    lexicogVals <- DF$lexicogName[-which(duplicated(DF$lexicogName))]
  }else{
    lexicogVals <- DF$lexicogName
  }
  SharableEntry <- lapply(lexicogVals, function(x) GetSharableEntryElement(x,Entry,DF) )
  names(SharableEntry) <- unlist(lapply(lexicogVals, function(x) unlist(str_split(x,":"))[2]))
  SharableEntryJson <- toJSON(SharableEntry, pretty=T)
  SharableEntryJson <- paste("{",context,SharableEntryJson ,sep="\n")
  SharableEntryJson <- gsub('\\{(\n\\s+)(\"LexicalEntry\":)',paste0('\\1"LexicographicResource\": "', credits ,'"\\1\\2,'), SharableEntryJson)
  write(SharableEntryJson, paste0("./SharableDictData/",SafeEncodeUTF8(names(Entry)),".json"))

}

