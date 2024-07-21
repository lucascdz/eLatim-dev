
# functions to convert conllu files to DF and extract metadata from conlu # tags

# CorpusConlluToDF() returns a list of 2:
# elemnt 1 is a DF containing the vertical corpus
# element 2 is a metadata DF containing the # tags extracted from the conllu file (e.g. sent_id, source etc)

require(readtext)
require(tidyverse)

ElementCounter <- function(Text, elementPattern){


  n <- str_count(Text, paste0(' ', elementPattern, ' '))
  count_id <- 1
  while (count_id <= n) {
    Text <- str_replace(Text, paste0(' ', elementPattern, ' '),  paste0(' ', count_id, ' '))
    count_id <- count_id+1
  }
  #write(String, file = paste0(TargetDirectory,"/",TextName,"WithCountId.txt"))
  return(Text)
}

BuildMetaDF <- function(MetaVal){
  MetaVal <- gsub("\\s+=\\s+","=",MetaVal)
  MetaVal <- gsub("^\\s+","",MetaVal)
  Attrs <- unlist(str_extract_all(MetaVal, "(^|\\s)[^\\s]*?=\\s?"))
  Attrs <- gsub("\\s","", Attrs)
  if(length(Attrs)>1){
    AttrsVals <- unlist(str_split(MetaVal, paste(Attrs, collapse = "|")))#[-1] # the first is always the xml elements, e.g. doc, so we remove it
    AttrsVals <- AttrsVals[AttrsVals!=""]
  }else if(length(Attrs)==1){
    AttrsVals <- unlist(str_split(MetaVal, Attrs))
    AttrsVals <- AttrsVals[AttrsVals!=""]
  }
  MetaDF <- tibble(!!!setNames(gsub(">","",AttrsVals), gsub("\\s|=","",Attrs)))
  MetaDF <- data.frame(MetadataString=MetaVal,MetaDF)
  return(MetaDF)
}


extractMetadataDFfromConllu <- function(conlluSent,MetadataTag,filename,i){
  conlluSent <- gsub(",","",conlluSent)
  conlluElements <- unlist(str_extract_all(conlluSent, "#.*?\n"))
  conlluElements <- gsub( "\n|#","",conlluElements)

  conlluElements <- conlluElements[str_detect(conlluElements, MetadataTag)]

  MetaTib <- lapply(conlluElements, function(x) BuildMetaDF(x) )

  MetaDF <- do.call(cbind, MetaTib)

  MetaDF <- data.frame(filename=filename, MetaDF, sID=i)

}

conlluSentToDF <- function(conlluSent, SentenceIDtag,columnNames,index){


  sent_id <- str_extract(conlluSent,paste0(SentenceIDtag,"\\s?[=:]\\s?.*?\n"))
  if(!is.na(sent_id)){
    sent_id <- str_remove_all(sent_id,paste0("\n|", SentenceIDtag,"|[=:]|\\s"))
    #print(sent_id) # uncomment this line for debugging purposes
    NoTagsSent <- str_remove_all(conlluSent, "#.*?\n")
    NoTagsSent <- str_replace_all(NoTagsSent,",",";;") # to avoid interfering with read.table delimit
    NoTagsSent <- str_replace_all(NoTagsSent,"'","%%") # to avoid interfering with read.table delimit
    NoTagsSent <- str_replace_all(NoTagsSent,'"',"@@") # to avoid interfering with read.table delimit
    NoTagsSent <- gsub("(^|\n|\r)\\d+-\\d+.*?\n","\n",NoTagsSent)

    SentDF <- read.table(text=NoTagsSent, header=F, fill=T)#read.table(text=NoTagsSent,col.names=columnNames)

    SentDF[[SentenceIDtag]] <- rep(sent_id,nrow(SentDF))

    SentDF <- SentDF[!str_detect(SentDF[[colnames(SentDF)[1]]],"-"),] # removes rows with tokenisation span info
    if (nrow(SentDF)>1 ){
      SentDF <- as.data.frame(sapply(SentDF, as.character ))

    }else{
      for(j in 1:ncol(SentDF)){
        SentDF[,j] <- as.character(SentDF[,j]) # sapply does not work correctly on single-row DFs
      }
    }

    # if(TRUE %in% !str_detect(SentDF[[colnames(SentDF)[1]]], "\\D")){
    #   # sanitize ID column, in case the digits are characters
    #   # if the col contains letters this line will transform them into NA:
    #   # check that the ID col is only digits! before running this
    #   SentDF$V1 <- as.numeric(SentDF[[colnames(SentDF)[1]]])
    # }

    SentDF[[colnames(SentDF)[1]]] <- as.character(SentDF[[colnames(SentDF)[1]]])
    SentDF <- SentDF[,c(1:(length(columnNames)), length(SentDF)) ] # trims back CorpusDF to the desired length, in case some extra columns were filled due to uneven number of tabs
    colnames(SentDF) <- c(columnNames, SentenceIDtag)
    SentDF$sID <- index
    return(SentDF)
  }
}


CorpusConlluToDF <- function(HeadwordVar,SourceDir,filename,columnNames, SentenceBoundaryMarker, SentenceIDtag, MetadataTag = NULL ,TargetDir){
  # this function requires conluu-style vertical corpus (one token per line) with sentence boundaries and sentnece identifiers
  # SentenceBoundaryMarker is the hashtagged string at the very top of a sentence block * WITHOUTH THE HASHTAG *
  # the default SentenceBoundaryMarker is set to sent_id
  # SentenceIDtag specifies the hashtagged string corresponding to unique sentence identifier,
  # it may or not be the same as the SentenceBoundaryMarker

  # load conllu file as txt

  Corpus <- readtext(paste0(SourceDir, "/",filename))
  Corpus <- Corpus$text

  #Corpus <- gsub("^(#.*?\n)+#\\s?sent_id", "# sent_id", Corpus) # standardize # sent_id tag
  #Corpus <- gsub(paste0("^(#.*?\n)+#\\s?",SentenceBoundaryMarker), paste0("# ",SentenceBoundaryMarker),Corpus) # standardizes SentenceBoundaryMarker
  Corpus <- gsub("\t#\t#\t", "\thashtagsymbol\thashtagsymbol\t", Corpus) # read.table does not read lines with # correctly

  # split corpus by sentence
  if (str_detect(Corpus,paste0('#\\s?',SentenceBoundaryMarker,'\\s?[=:]\\s?') )){
    CorpusSents <- unlist(str_split(Corpus, paste0('#\\s?',SentenceBoundaryMarker,'\\s?[=:]\\s?')))
    CorpusSents <- CorpusSents[CorpusSents!=""]
    CorpusSents <- CorpusSents[CorpusSents!="\n"]
    CorpusSents <- CorpusSents[!str_detect(CorpusSents, "^#")]

#print("line 123")
  # clean up: remove unnecessary info and isolate sentences (this removes all metadata before sent_id.
  # it is recommended to split the document at # newdoc_id before running this function if there are multiple documents in a single file )
  CorpusSents <- lapply(CorpusSents, function(x) str_remove_all(x,"^\\s?#.*?\n(\\s?#.*?\n)+") )
  CorpusSents <- lapply(CorpusSents, function(x) str_remove_all(x,"\n(\n\\s?#.*?\n)(\n?\\s?#.*?\n)?$") ) # this removes any tags wrongly merged to previous sentence
  CorpusSents <- CorpusSents[sapply(CorpusSents, function(x) x!="")] # filters out empty sentences

  # put back sent_id tag:
  CorpusSents <- paste0('# ', SentenceBoundaryMarker,' = ', CorpusSents)


  CorpusSentDFs <- lapply(seq_along(CorpusSents), function(i) conlluSentToDF(CorpusSents[i],SentenceIDtag,columnNames,i))
  CorpusSentDFs <- CorpusSentDFs[!sapply(CorpusSentDFs, is.null)]
  CorpusDF <- do.call(bind_rows, CorpusSentDFs)

  CorpusDF[[colnames(CorpusDF)[2]]][CorpusDF[[colnames(CorpusDF)[2]]]=="%%"] <- "'"  # re-convert punctuation to its original form
  CorpusDF[[colnames(CorpusDF)[2]]][CorpusDF[[colnames(CorpusDF)[2]]]==";;"] <- ","  # re-convert punctuation to its original form
  CorpusDF[[colnames(CorpusDF)[2]]][CorpusDF[[colnames(CorpusDF)[2]]]=="@@"] <- '"'  # re-convert punctuation to its original form
  CorpusDF[[colnames(CorpusDF)[3]]][CorpusDF[[colnames(CorpusDF)[3]]]=="%%"] <- "'"  # re-convert punctuation to its original form
  CorpusDF[[colnames(CorpusDF)[3]]][CorpusDF[[colnames(CorpusDF)[3]]]==";;"] <- ","  # re-convert punctuation to its original form
  CorpusDF[[colnames(CorpusDF)[3]]][CorpusDF[[colnames(CorpusDF)[3]]]=="@@"] <- '"'  # re-convert punctuation to its original form

  #print("line 145")
  write.fst(CorpusDF, paste0(TargetDir,"/CorpusDocs/",gsub(".conllu$",".fst", filename)))
  #print("line 146")

  #extract Freqs
  if(length(HeadwordVar)>1){
    compHead <- as.character(paste(HeadwordVar, collapse = "_"))
    CorpusDF[[sym(compHead)]] <- paste(CorpusDF[[HeadwordVar[1]]],CorpusDF[[HeadwordVar[2]]],sep="_")
    HeadwordVar <- compHead
  }
  CorpusDF[[HeadwordVar]][is.na(CorpusDF[[HeadwordVar]])] <- ""
  headwords <- unique(CorpusDF[[HeadwordVar]][CorpusDF[[HeadwordVar]]!=""])
  
  if (Sys.info()[['sysname']]!="Windows"){	
    headwordsIndex <- mclapply(headwords, function(x) CorpusDF$sID[CorpusDF[[HeadwordVar]]==x])	
    
  }else{
    
    
    
    
    cl <- makeCluster(as.numeric(Cores))


    headwordsIndex <- parLapply(cl, headwords, function(x) CorpusDF$sID[CorpusDF[[HeadwordVar]]==x])
    stopCluster(cl)
  }	
  
  names(headwordsIndex) <- headwords

  saveRDS(headwordsIndex, paste0(TargetDir,"/CorpusData/HeadwordIndex_",gsub(".conllu$","",filename),".rds"))
  TextLemmaFreqs <- as.data.frame(table(CorpusDF[[HeadwordVar]][CorpusDF[[HeadwordVar]]!=""]))
  colnames(TextLemmaFreqs)[1] <- HeadwordVar
  write_fst(TextLemmaFreqs, paste0(TargetDir,"/OutputFreqs/HeadwordFreqs_",gsub(".conllu$","",filename),".fst"))

  CorpusDF$RowID <- 1:nrow(CorpusDF)
  if (Sys.info()[['sysname']]!="Windows"){	
    sIdIndexDF <- mclapply(split(CorpusDF, CorpusDF$sID), function(x) data.frame(sID=x$sID[1],Start = x$RowID[1], End = x$RowID[length(x$RowID)]))	
    
  }else{
    
    
    
    cl <- makeCluster(as.numeric(Cores))
    clusterExport(cl, c('Cores'))
    clusterEvalQ(cl, {
      
      
      source("./R/SetUp.R")
      
      
    })
    
    sIdIndexDF <- parLapply(cl, split(CorpusDF, CorpusDF$sID), function(x) data.frame(sID=x$sID[1],Start = x$RowID[1], End = x$RowID[length(x$RowID)]))
    stopCluster(cl)
  }	
  
  sIdIndexDF <- do.call(rbind, sIdIndexDF)
  write.fst(sIdIndexDF, paste0("./data/CorpusData/", "sIDindex_",gsub(".conllu$","",filename),".fst" ))
  CorpusDF <- CorpusDF[,colnames(CorpusDF)!="RowID"]
  #print("line 166")
  # extract Meta

  if(!is.null(MetadataTag) && length(MetadataTag)>0){
print(paste("MetadataTag",MetadataTag))
    MetaDFs <-  lapply(seq_along(CorpusSents), function(i)  extractMetadataDFfromConllu(CorpusSents[i], MetadataTag, filename,i))
    CorpusMeta <- do.call(bind_rows, MetaDFs) # use bind_rows instead of rbind to allow for missing attributes
    CorpusMetaSplit <- split(CorpusMeta, CorpusMeta$MetadataString)
    if (Sys.info()[['sysname']]!="Windows"){	
      CorpusMeta <- mclapply(CorpusMetaSplit, function(x) data.frame(x[1,colnames(x)!="sID"],sIDsStart = x$sID[1], sIDsEnd = x$sID[length(x$sID)]))	
      
    }else{
      
      
      
      
      cl <- makeCluster(as.numeric(Cores))
      clusterExport(cl, c('Cores'))
      clusterEvalQ(cl, {
        
        
        source("./R/SetUp.R")
        
        
      })

      CorpusMeta <- parLapply(cl, CorpusMetaSplit, function(x) data.frame(x[1,colnames(x)!="sID"],sIDsStart = x$sID[1], sIDsEnd = x$sID[length(x$sID)]))
      stopCluster(cl)
    }
    
    
    CorpusMeta <- do.call(rbind, CorpusMeta)
    row.names(CorpusMeta) <- 1:nrow(CorpusMeta)
    gc()
    return(CorpusMeta)
  }else{
    gc()
    return(NULL)
  }

  }else{
    print("cannot detect sentnece boundaries")
  }

}




