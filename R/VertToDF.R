
# script to convert vertical corpus files with xml tags (.vert/.vrt/.cqp) to DF and fst
# and extract freqs from them



source("./R/FunctionToCreateCorpusObjects.R")
source("./R/SetUp.R")

AddsIDCol <- function(Text, SentEndMarker, ColWithSentMarker, fileName,Cores=2){

  # SentEndMarker can be a closing sentnece tag, e.g. </s> or punctuation, e.g. '.' , '?', '//' ...
  # ColWithSentMarker is the name of the column containing the SentEndMarker, e.g. 'word', 'FORM'..
  # when processing vert files read with read.delim, ColWithSentMarker is always the first column

  SentEndIndex <- which(str_detect(Text[[ColWithSentMarker]], SentEndMarker))
  SentStartIndex <- c(1,(SentEndIndex[-length(SentEndIndex)])+1) # sentneces may include xml tags, the first will likely include header

  if(length(SentEndIndex)==length(SentStartIndex)){


    if (Sys.info()[['sysname']]!="Windows"){
      TextSents <- mclapply(seq_along(SentStartIndex), function(i)  SplitSentsAndAddsIDCol(Text,i,SentStartIndex,SentEndIndex),mc.cores=Cores)

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

      TextSents <- parLapply(cl, seq_along(SentStartIndex), function(i)  SplitSentsAndAddsIDCol(Text,i,SentStartIndex,SentEndIndex))

      stopCluster(cl)

    }





  TextwithsID <- do.call(rbind, TextSents)
  TextwithsID[is.na(TextwithsID)] <- ""

  # write sIDindex for Text
  write.fst(data.frame(sID=seq_along(SentStartIndex), Start=SentStartIndex, End=SentEndIndex), paste0("./data/CorpusData/", "sIDindex_",gsub(".(ve?rt|cqp)$","",fileName),".fst" ))

  # sIDIndex <- lapply(unique(TextwithsID$sID), function(x) data.frame(sID=x, Start=which(TextwithsID$sID==x)[1], End=which(TextwithsID$sID==x)[length(which(TextwithsID$sID==x))] ))
  # sIDIndex <- do.call(rbind,sIDIndex)
  # write.fst(sIDIndex, paste0("./data/CorpusData/", "sIDindex_",gsub(".(ve?rt|cqp)$","",fileName),".fst" ))

  return(TextwithsID)
  }
}

ConvertTagsToCol <- function(Text, ColWithSentMarker, TagsVector,Cores=2){
  # TagsVector should list the name of XML elements that one want to retain as separate column
  # without the <> , e.g. "s", "doc", "page"
  # each listed XML element must have matching closing tags (</s>, </doc>, </page>)
  # elements with mismatched closing tags will be skipped
  # the process of converting tags to columns is time consuming, so best to keep the list as short as possible
  # this function is mainly intended to be used with <page> and other text information that should be preserved for dictioanry use

  for(Tag in TagsVector){
    #print(Tag)
    Text <- TagToCol(Text, Tag, ColWithSentMarker,Cores)

  }

  Text <- Text[,names(Text)!="RowID"]

}

AddAttributeCol <- function(Text,i, Tag, SentStartIndex,SentEndIndex,ColWithSentMarker){

    TextSeg <-  Text[SentStartIndex[i]:SentEndIndex[i],]
    TextSeg[[Tag]] <- TextSeg[[ColWithSentMarker]][1]

    return(TextSeg)

  }

TagToCol <- function(Text, Tag, ColWithSentMarker,Cores=2){

  #print(Tag)

  SentEndIndex <- which(str_detect(Text[[ColWithSentMarker]], paste0("</\\s?",Tag,"\\s?>")))
  SentStartIndex <- which(str_detect(Text[[ColWithSentMarker]], paste0("<\\s?",Tag,"(\\s|>)")))
if(length(SentEndIndex)==(length(SentStartIndex)-1)){
  SentEndIndex <- c(SentEndIndex,nrow(Text))
}

  if(length(SentEndIndex)==length(SentStartIndex)){

    Text[[Tag]] <- ""
    Text$RowID <- 1:nrow(Text)



    if (Sys.info()[['sysname']]!="Windows"){
      TextSegs <- mclapply(seq_along(SentStartIndex), function(i) AddAttributeCol(Text, i, Tag, SentStartIndex,SentEndIndex,ColWithSentMarker), mc.cores=Cores)

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

      TextSegs <- parLapply(cl, seq_along(SentStartIndex), function(i) AddAttributeCol(Text, i, Tag, SentStartIndex,SentEndIndex,ColWithSentMarker))

      stopCluster(cl)

    }




    TextSegs <- do.call(rbind, TextSegs)

    Text <- Text[!Text$RowID %in% TextSegs$RowID,]
    Text <- rbind(Text, TextSegs)
    Text <- Text[order(as.numeric(Text$RowID), decreasing = F),]
  }else{
    print(paste("non matching opening and closing tags for ", Tag))
  }
  return(Text)
}

GetMetaIndex <- function(TextwithsID, MetaVal, MetadataTag, fileName){
  fileName <- gsub("\\..{2,4}$","",fileName)
  sIDs <- TextwithsID$sID[TextwithsID[[MetadataTag]]==MetaVal]
  #Attrs <- unlist(str_extract_all(MetaVal, " .*?\\s?="))
  MetaVal <- gsub("\\s+=\\s+","=",MetaVal)
  Attrs <- unlist(str_extract_all(MetaVal, "(^|\\s)[^\\s]*?=\\s?"))
  Attrs <- gsub("\\s","", Attrs)
  AttrsVals <- unlist(str_split(MetaVal, paste(Attrs, collapse = "|")))[-1] # the first is always the xml elements, e.g. doc, so we remove it
  MetaDF <- tibble(!!!setNames(gsub(">","",AttrsVals), gsub("\\s|=","",Attrs)))
  return(data.frame(filename=fileName, MetadataString=MetaVal, MetaDF, sIDsStart = sIDs[1], sIDsEnd = sIDs[length(sIDs)]-1)) # -1 because usually the SentBoundary element (e.g. <s>) in nested inside the metadata-containing element (e.g. <doc>) and therefore the real last <s> pertainign to a given <doc> is the next to last to appear in the sIDs vector

}

MakeCorpusTablesFromVertAndReturnMeta <- function(HeadwordVar, CorpusDir,  fileName, columnNames, SentEndMarker, TagsVector=NULL, MetadataTag=NULL, TargetDir="./data",Cores=2){


  # SentEndMarker can be a closing sentnece tag, e.g. </s> or punctuation, e.g. '.' , '?', '//' ...

  # TagsVector should list the name of XML elements that one want to retain as separate column
  # without the <> , e.g. "s", "doc", "page"
  # each listed XML element must have matching closing tags (</s>, </doc>, </page>)
  # elements with mismatched closing tags will be skipped
  # the process of converting tags to columns is time consuming, so best to keep the list as short as possible
  # this function is mainly intended to be used with <page> and other text information that should be preserved for dictioanry use

  print(fileName)
  Text <- read.delim(paste0(CorpusDir,"/",fileName), stringsAsFactors = F, header = F)
  colnames(Text) <- columnNames

  TextwithsID <- AddsIDCol(Text, SentEndMarker, columnNames[1] ,fileName, Cores=Cores)


  if(length(HeadwordVar)>1){
    compHead <- as.character(paste(HeadwordVar, collapse = "_"))
    TextwithsID[[sym(compHead)]] <- paste(TextwithsID[[HeadwordVar[1]]],TextwithsID[[HeadwordVar[2]]],sep="_")
    HeadwordVar <- compHead
  }


  if(!is.null(TagsVector) && length(TagsVector)>0){

    TextwithsID <- ConvertTagsToCol(TextwithsID,columnNames[1],TagsVector, Cores=Cores)
  }

  if(!is.null(MetadataTag) && length(MetadataTag)>0){

    TextwithsID <- ConvertTagsToCol(TextwithsID,columnNames[1],MetadataTag)
    MetaVals <- unique(TextwithsID[[MetadataTag]])
    MetaValsDF <- lapply(MetaVals[MetaVals!=""], function(x) GetMetaIndex(TextwithsID, x, MetadataTag, fileName))
    MetaValsDF <- do.call(rbind, MetaValsDF)
    TextwithsID <- TextwithsID[,colnames(TextwithsID)!=MetadataTag]

  }


 # TextwithsID <- TextwithsID[!str_detect(TextwithsID[[columnNames[1]]],"<.*?>"),]


  write_fst(TextwithsID, paste0(TargetDir,"/CorpusDocs/",gsub(".(ve?rt|cqp)$","",fileName),".fst"))

  #exctract Freqs

  TextwithsID[[HeadwordVar]][is.na(TextwithsID[[HeadwordVar]])] <- ""
  headwords <- unique(TextwithsID[[HeadwordVar]][TextwithsID[[HeadwordVar]]!=""])

  if (Sys.info()[['sysname']]!="Windows"){
    headwordsIndex <- mclapply(headwords, function(x) TextwithsID$sID[TextwithsID[[HeadwordVar]]==x], mc.cores=Cores)


  }else{
    #print('hello')
    #headwordsIndex <- mclapply(headwords, function(x) TextwithsID$sID[TextwithsID[[HeadwordVar]]==x])
    cl <- makeCluster(as.numeric(Cores))
    #clusterExport(cl, c('TextwithsID', 'HeadwordVar', 'headwords'))
    headwordsIndex <- parLapply(cl, headwords, function(x) TextwithsID$sID[TextwithsID[[HeadwordVar]]==x])
    stopCluster(cl)
  }






  names(headwordsIndex) <- headwords

  saveRDS(headwordsIndex, paste0(TargetDir,"/CorpusData/HeadwordIndex_",gsub(".(ve?rt|cqp)$","",fileName),".rds"))
  TextLemmaFreqs <- as.data.frame(table(TextwithsID[[HeadwordVar]][TextwithsID[[HeadwordVar]]!=""]))
  colnames(TextLemmaFreqs)[1] <- HeadwordVar
  write_fst(TextLemmaFreqs, paste0(TargetDir,"/OutputFreqs/HeadwordFreqs_",gsub(".(ve?rt|cqp)$","",fileName),".fst"))

  # extract Meta

  if(!is.null(MetadataTag) && length(MetadataTag)>0 && !is.null(MetaValsDF) && nrow(MetaValsDF)>0){

    return(MetaValsDF) # once all corpus files are processed, rbinf the MetaValsDF and write it to file, putting CorpusName in filename, in case one uses multiple corpora, whcih would yield MetaDFs with different cols
  }

}

# To Test on Ranka's files:
# CorpusDir <- "/Users/ligeialugli/Dropbox/DemocratizingDigitialLexi/Participants_CorpusSamples/Ranka_Italian-Serbian-Aligned-Corpus-vert"
# fileName <- "IT-XML-NER.cqp"
# columnNames <- c("word","TMX","PoS","lemma","id")
# SentEndMarker <- "</seg>"
# ColWithSentMarker <- columnNames[1]
# TagsVector <- c("pers", "loc")
# MetadataTag <- "doc"

# to Test on Yuri's:
# CorpusDir <- "/Users/ligeialugli/Dropbox/DemocratizingDigitialLexi/Participants_CorpusSamples/Yuri"
# HeadwordVar <- "lemma"
# columnNames <- c("ID","word","lemma",	"Pos"	,"X","Y","Z")
# CorpusName <- "Yuri"
# SentEndMarker <- "</s>"
# TagsVector <- NULL
# MetadataTag <- "doc"
# Cores <-3

# to Test
# instructions <- read.csv("./data/dataExtractionInstructions.csv", stringsAsFactors = F)
# colnames(instructions)
#  CorpusDir <- instructions$CorpusDir[2]
#  instructions$columnNames[2]
#  HeadwordVar <- "lemma"
#  columnNames <- c("WordID", "word", "NormSTEM", "suffix" ,"lemma", "PoS", "Root", "NormSuffix", "NormWord")
#  CorpusName <- instructions$CorpusName[2]
#  SentEndMarker <- instructions$SentEndMarker[2]
#  TagsVector <- NULL
#  MetadataTag <- NULL
#  Cores <- 3
# ColWithSentMarker <- "word"


