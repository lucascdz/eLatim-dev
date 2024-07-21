
# script to create corpus-derived tables from csv corpus documents

SplitSentsAndAddsIDCol <- function(Text,i,SentStartIndex,SentEndIndex){
  #print(i)
  Text <-  Text[SentStartIndex[i]:SentEndIndex[i],]
  Text$sID <- i
  return(Text)
}

AddsIDCol <- function(Text, SentEndMarker, ColWithSentMarker, fileName, mcCores=3){

  # print(Text[5:10,])
  SentEndIndex <- which(str_detect(Text[[ColWithSentMarker]], SentEndMarker))
  if(length(SentEndIndex)<1){
    SentEndIndex <- nrow(Text)
  }
  SentStartIndex <- c(1,(SentEndIndex[-length(SentEndIndex)])+1)

  if (Sys.info()[['sysname']]!="Windows"){
    TextSents <- mclapply(seq_along(SentStartIndex), function(i)  SplitSentsAndAddsIDCol(Text,i,SentStartIndex,SentEndIndex), mc.cores=mcCores)

  }else{



    cl <- makeCluster(as.numeric(Cores))
    print(as.numeric(Cores))
    #registerDoParallel(cl)
    #setDefaultCluster(cl)
    clusterExport(cl, c('Cores'))
    clusterEvalQ(cl, {

      source("./R/VertToDF.R")
      source("./R/SetUp.R")
      source("./R/ConlluToDF.R")
      source("./R/CorpusFreqsAndDPfunctions.R")
      source("./R/flextextToDF.R")
      source("./R/FunctionToCreateCorpusObjects.R")
      source("./R/TxtToDF.R")

    })

    TextSents <- parLapply(cl, seq_along(SentStartIndex), function(i)  SplitSentsAndAddsIDCol(Text,i,SentStartIndex,SentEndIndex))
    stopCluster(cl)

  }





  TextwithsID <- do.call(rbind, TextSents)
  TextwithsID[is.na(TextwithsID)] <- ""

  # write sIDindex for Text
  write.fst(data.frame(sID=seq_along(SentStartIndex), Start=SentStartIndex, End=SentEndIndex), paste0("./data/CorpusData/", "sIDindex_",gsub(".csv$","",fileName),".fst" ))

  return(TextwithsID)
}

BuildMetaDFfromCSV <- function(MetaVal, sidStart, sidEnd){

  MetaVal <- gsub("\\s+=\\s+","=",MetaVal)
  Attrs <- unlist(str_extract_all(MetaVal, "(^|\\s)[^\\s]*?=\\s?"))
  Attrs <- gsub("\\s","", Attrs)
  AttrsVals <- unlist(str_split(MetaVal, paste(Attrs, collapse = "|")))[-1] # the first is always the xml elements, e.g. doc, so we remove it
  MetaDF <- tibble(!!!setNames(gsub(">","",AttrsVals), gsub("\\s|=","",Attrs)))
  return(data.frame(MetadataString=MetaVal, MetaDF, sIDsStart = sidStart, sIDsEnd = sidEnd))

}



MakeCorpusTables <- function(HeadwordVar,CorpusDir, fileName, SentEndMarker,ColWithSentMarker, MetadataTag=NULL, ColWithMetadataTag=NULL, TargetDir, mcCores=3){

print(fileName)

  TextwithsID <- AddsIDCol( fread(paste0(CorpusDir,"/",fileName)), SentEndMarker, ColWithSentMarker,fileName)


  #print('line 49')
  if(length(HeadwordVar)>1){
    TextwithsID <- unite(TextwithsID, paste(HeadwordVar, collapse = "_"), HeadwordVar, remove=F)
    HeadwordVar <- paste(HeadwordVar, collapse = "_")
  }
  #print('line 54')
  if(nrow(TextwithsID[TextwithsID[[HeadwordVar]]!=""])>0){
  #print('line 56')
  write_fst(TextwithsID, paste0(TargetDir,"/CorpusDocs/",gsub(".csv$","",fileName),".fst"))

  TextwithsID[[HeadwordVar]][is.na(TextwithsID[[HeadwordVar]])] <- ""
  headwords <- unique(TextwithsID[[HeadwordVar]][TextwithsID[[HeadwordVar]]!=""])



  if (Sys.info()[['sysname']]!="Windows"){
    headwordsIndex <- mclapply(headwords, function(x) TextwithsID$sID[TextwithsID[[HeadwordVar]]==x],mc.cores= mcCores)

  }else{

    # mclapply(dir(CorpusDir), function(x) MakeCorpusTablesFromFlexAndReturnMeta(HeadwordVar,"doc" , CorpusDir, x, TargetDir, csvTargetDir))
    # print(mclapply(dir(CorpusDir), function(x) MakeCorpusTablesFromFlexAndReturnMeta(HeadwordVar,"doc" , CorpusDir, x, TargetDir, csvTargetDir)))

    cl <- makeCluster(as.numeric(Cores))
    print(as.numeric(Cores))
    #registerDoParallel(cl)
    #setDefaultCluster(cl)
    clusterExport(cl, c('Cores'))
    clusterEvalQ(cl, {

      source("./R/VertToDF.R")
      source("./R/SetUp.R")
      source("./R/ConlluToDF.R")
      source("./R/CorpusFreqsAndDPfunctions.R")
      source("./R/flextextToDF.R")
      source("./R/FunctionToCreateCorpusObjects.R")
      source("./R/TxtToDF.R")

    })

    headwordsIndex <- parLapply(cl, headwords, function(x) TextwithsID$sID[TextwithsID[[HeadwordVar]]==x])
    #print(parLapply(cl, dir(CorpusDir), function(x) MakeCorpusTablesFromFlexAndReturnMeta(HeadwordVar,"doc" , CorpusDir, x, TargetDir, csvTargetDir)))
    stopCluster(cl)

  }


  names(headwordsIndex) <- headwords

  saveRDS(headwordsIndex, paste0(TargetDir,"/CorpusData/HeadwordIndex_",gsub(".csv$","",fileName),".rds"))
  TextLemmaFreqs <- as.data.frame(table(TextwithsID[[HeadwordVar]][TextwithsID[[HeadwordVar]]!=""]))
  colnames(TextLemmaFreqs)[1] <- HeadwordVar
  write_fst(TextLemmaFreqs, paste0(TargetDir,"/OutputFreqs/HeadwordFreqs_",gsub(".csv$","",fileName),".fst"))


  # extract Meta

  if(!is.null(MetadataTag)){
    sIDsStart <- TextwithsID$sID[str_detect(TextwithsID[[ColWithMetadataTag]], paste0("<\\s?",MetadataTag) )]
    sIDsEnd <- TextwithsID$sID[str_detect(TextwithsID[[ColWithMetadataTag]], paste0("</\\s?",MetadataTag,"\\s?>") )]
    if(length(sIDsStart) ==  (length(sIDsEnd)+1)){
      sIDsEnd <- c(sIDsEnd, nrow(TextwithsID))
    }
    MetaVal <- TextwithsID[[ColWithMetadataTag]][str_detect(TextwithsID[[ColWithMetadataTag]], paste0("<\\s?",MetadataTag) )]
    CorpusMeta <- lapply(seq_along(sIDsStart), function(i) BuildMetaDFfromCSV(MetaVal= MetaVal[i], sidStart= sIDsStart[i], sidEnd =sIDsEnd[i] ) )
    CorpusMeta <- data.frame(filename=fileName, CorpusMeta)
    return(CorpusMeta)
  }
}
}

# CorpusDir <- "/Users/ligeialugli/Dropbox/BTW_Master/BuddhLemm/NormSuff"
# fileName <- "acintyastava.csv"
# SentEndMarker <- "</s>"
# ColWithSentMarker<- "word"
# MetadataTag <- "doc"
# ColWithMetadataTag <- "word"
# HeadwordVar <- "lemma"

# to Test on BuddhCorpus's:
# CorpusDir <- "/Users/ligeialugli/Dropbox/BTW_Master/BTW_CorpusStuff/BTW_CorpusData/GoldishCorpusBuddh"
# HeadwordVar <- "lemma"
# CorpusName <- "SktFullBuddhCorpus2"
# SentEndMarker <- "</s>"
# ColWithSentMarker <- "word"
# MetadataTag <- NULL
# Cores <- 6
#
# lapply(paste0(todo,".csv"), function(x) MakeCorpusTables(HeadwordVar, CorpusDir,  fileName=x, SentEndMarker, ColWithSentMarker, MetadataTag=NULL, ColWithMetadataTag=NULL, TargetDir="./data") )


