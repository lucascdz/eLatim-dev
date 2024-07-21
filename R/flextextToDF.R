source("./R/VertToDF.R") # needed for metadata extraction functions


GetFlexMetaIndex <- function(TextwithsID, MetaVal, MetadataTag, DocItemNo, fileName){
  fileName <- paste(gsub(".flextext" ,"",fileName), DocItemNo, sep="_")
  sIDs <- TextwithsID$sID[TextwithsID[[MetadataTag]]==MetaVal]
  #Attrs <- unlist(str_extract_all(MetaVal, " .*?\\s?="))
  MetaVal <- gsub("\\s+=\\s+","=",MetaVal)
  Attrs <- unlist(str_extract_all(MetaVal, "(^|\\s)[^\\s]*?=\\s?"))
  Attrs <- gsub("\\s","", Attrs)
  AttrsVals <- unlist(str_split(MetaVal, paste(Attrs, collapse = "|")))[-1] # the first is always the xml elements, e.g. doc, so we remove it
  MetaDF <- tibble(!!!setNames(gsub(">","",AttrsVals), gsub("\\s|=","",Attrs)))
  MetaVal <- gsub('"','',MetaVal)
  return(data.frame(filename=fileName, MetadataString=MetaVal, MetaDF, sIDsStart = sIDs[1], sIDsEnd = sIDs[length(sIDs)]-1)) # -1 because usually the SentBoundary element (e.g. <s>) in nested inside the metadata-containing element (e.g. <doc>) and therefore the real last <s> pertainign to a given <doc> is the next to last to appear in the sIDs vector

}


GetVertSentDF <- function(sentDF){

  DF <- sentDF[,colnames(sentDF) %in% c("sID", "txt", "cf", "gls", "msa", "morph")]
  if(length(unique(sentDF$free_trans))){
    FirstRow <- data.frame(sID=unique(sentDF$sID), txt= paste0('<s number= "',unique(sentDF$sID), ' translation= "', unique(sentDF$free_trans) ,'">'), cf="", gls="", msa="", morph="")
  }else{
    FirstRow <- data.frame(sID=unique(sentDF$sID), txt=paste0('<s number= "',unique(sentDF$sID),' translation= "">'), cf="", gls="", msa="", morph="")
  }

  LastRow <- data.frame(sID=unique(sentDF$sID), txt="</s>", cf="", gls="", msa="", morph="")
  DF <- rbind(FirstRow, DF, LastRow)
  DF$wordID <- 1:nrow(DF)
  return(DF)
}


GetVertFromText_df <- function(text_df,AbbrTitle,Source, DocItemNo, fileName, TargetDir,csvTargetDir){
  rownames(text_df) <- seq_along(text_df$text)
  text_df <- as.data.frame(apply(text_df, 2, function(x) {
    ifelse(is.na(x), "", x)
  }))
  text_df$sID <- as.numeric(text_df$sID)

  Vert <- split(text_df,  text_df$sID)


  VertDFs <- lapply(Vert, function(x) GetVertSentDF(x))
  VertText <- do.call(rbind,VertDFs)


  FirstRow <- data.frame(wordID=0,sID=0, txt=paste0('<doc title= "',Vert[1][[1]]$text_title[1],'" abbreviated_title="', AbbrTitle ,'" source="',Source ,'">'), cf="", gls="", msa="", morph="")
  LastRow <- data.frame(wordID=NA, sID=max(VertText$sID)+1, txt="</doc>", cf="", gls="", msa="", morph="")

  VertText <- rbind(FirstRow,VertText,LastRow)
  VertText <- VertText[,c(3:length(VertText),1,2)]


  if(!is.null(csvTargetDir)){

    # print('paste0(csvTargetDir,"/", as.character(text_df$text_title[1]),".csv")')
    # print(paste0(csvTargetDir,"/", as.character(text_df$text_title[1]),".csv"))
    fileName <- gsub(".flextext","",fileName)
    fwrite(VertText, paste0(csvTargetDir,"/", fileName,"_",DocItemNo ,".csv"))

  }
  if(file.exists(paste0(TargetDir,"/CorpusDocs"))){

    # print("head(text_df)")
    # print(head(text_df))

    # writes fst file with streamlined versio of text_df for use in 'Democratizing Digital Lexicography' ecosystem
    text_df <- text_df[,colnames(text_df) %in% c("sID", "txt", "cf", "gls", "msa", "morph")]

    write.fst(text_df, paste0(TargetDir,"/CorpusDocs/", fileName,"_",DocItemNo , ".fst"))

  }
  return(VertText)
}


flextextToDF <- function (CorpusDir, fileName, TargetDir="./data", csvTargetDir) {

  # TargetDir is the destination of csv vertical file
  # the vert file starts with <doc> header, which contains 2 attributes: title and source.

  # this function is a modification of phonfieldwork::flextext_to_df
  # it assumes one file per titled text.

  # the col names are based on the xml tags in the file: "txt", "cf", "gls", "msa", "morph"; plus sID and wordID
  # note that wordID is the index of each word in the sentece, and does not correspond to FLEx's word ID

  l <- xml2::read_xml(paste0(CorpusDir,"/",fileName))
  l <- xml2::xml_find_all(l, "interlinear-text")
  items <- xml2::xml_find_all(l, "item")
  if(TRUE %in% str_detect(as.character(items),"source")){
    source <- str_extract(as.character(items[str_detect(as.character(items),"source")]), ">.*?<" )
    if(TRUE %in% !is.na(source)){
      source <- gsub("<|>","", source)
    }else{
      source <- "not available"
    }
  }else{
    source <- "not available"
  }
  if(TRUE %in% str_detect(as.character(items),"title-abbreviation")){
    abbrTitle <- str_extract(as.character(items[str_detect(as.character(items),"title-abbreviation")]), ">.*?<" )
    if(TRUE %in% !is.na(abbrTitle)){
      abbrTitle <- gsub("<|>","", abbrTitle)
    }else{
      abbrTitle <- "not available"
    }
  }else{
    abbrTitle <- "not available"
  }



  text_dfs <- lapply(seq_along(l), function(i) {
    t <- xml2::xml_find_all(l[[i]], "paragraphs/paragraph/phrases/phrase/words/word")
    if (length(t) == 0) {
      stop("It looks like this .flextext doesn't have any word-level annotations.")
    }
    result_df <- lapply(seq_along(t), function(j) {
      word <- xml2::xml_attr(t[[j]], "guid")
      p_at <- xml2::xml_attr(xml2::xml_children(xml2::xml_parents(t[[j]])[[2]]),
                             "type")
      free_trans <- ifelse("gls" %in% p_at, xml2::xml_text(xml2::xml_children(xml2::xml_parents(t[[j]])[[2]])[which(p_at %in%
                                                                                                                      "gls")]), "")
      lit_trans <- ifelse("lit" %in% p_at, xml2::xml_text(xml2::xml_children(xml2::xml_parents(t[[j]])[[2]])[which(p_at %in%
                                                                                                                     "lit")]), "")
      if (free_trans == "" & lit_trans == "") {
        free_trans <- ""
      }
      else if (free_trans != "" & lit_trans == "") {
        free_trans <- free_trans
      }
      else if (free_trans == "" & lit_trans != "") {
        free_trans <- lit_trans
      }
      else if (free_trans != "" & lit_trans != "") {
        free_trans <- paste0(free_trans, " (", lit_trans,
                             ")")
      }
      Title <- xml2::xml_text(xml2::xml_children(xml2::xml_parents(t[[j]])[[6]])[1])


      m <- xml2::xml_find_all(t[[j]], "morphemes/morph")
      if (length(m) == 0) {
        other <- unlist(xml2::xml_attrs(xml2::xml_parents(t[[j]]),
                                        "type"))
        data.frame(txt = xml2::xml_text(t[[j]]), cf = NA,
                   hn = NA, gls = NA, msa = NA, free_trans = free_trans,
                   text_title = Title, morph = NA, word = word,
                   phrase = other[1], paragraph = other[2], text = other[3])
      }
      else {
        morphemes <- lapply(m, function(morpheme) {
          morph <- ifelse(!is.na(xml2::xml_attr(morpheme, "type")),xml2::xml_attr(morpheme, "type"),xml2::xml_attr(morpheme, "guid")  )
          other <- unlist(xml2::xml_attrs(xml2::xml_parents(morpheme),
                                          "type"))
          values <- xml2::xml_text(xml2::xml_children(morpheme))
          attrs <- xml2::xml_attr(xml2::xml_children(morpheme),
                                  "type")
          data.frame(txt = ifelse("txt" %in% attrs, values[which(attrs ==
                                                                   "txt")], NA), cf = ifelse("cf" %in% attrs,
                                                                                             values[which(attrs == "cf")], NA), hn = ifelse("hn" %in%
                                                                                                                                              attrs, values[which(attrs == "hn")], NA),
                     gls = ifelse("gls" %in% attrs, values[which(attrs ==
                                                                   "gls")], NA), msa = ifelse("msa" %in% attrs,
                                                                                              values[which(attrs == "msa")], NA), free_trans = free_trans,
                     text_title = Title, morph = morph, word = other[1],
                     phrase = other[2], paragraph = other[3],
                     text = other[4])
        })
        do.call(rbind, morphemes)
      }
    })
    df <- do.call(rbind, result_df)
    df$word[which(is.na(df$word))] <- seq_along(which(is.na(df$word)))
    index <- data.frame(p_id = as.numeric(factor(df$paragraph,
                                                 levels = unique(df$paragraph))), sID = as.numeric(factor(df$phrase,
                                                                                                          levels = unique(df$phrase))), w_id = as.numeric(factor(df$word,
                                                                                                                                                                 levels = unique(df$word))))
    cbind(index, df)
  })

  #text_df <- do.call(rbind, text_df)

  AllVerts <- lapply(seq_along(text_dfs), function(i) GetVertFromText_df(text_dfs[i][[1]], abbrTitle[i], source[i], i, fileName, TargetDir, csvTargetDir))

  return(AllVerts)


}


GetDocMetaAndFreqs <- function(TextwithsID, HeadwordVar, MetadataTag=NULL, TargetDir="./data", DocItemNo, fileName){
  row.names(TextwithsID) <- 1:nrow(TextwithsID)

  if(!is.null(MetadataTag)){

    TextwithsID <- ConvertTagsToCol(TextwithsID,colnames(TextwithsID)[1],MetadataTag)
    MetaVals <- unique(TextwithsID[[MetadataTag]])
    MetaValsDF <- lapply(MetaVals[MetaVals!=""], function(x) GetFlexMetaIndex(TextwithsID, x, MetadataTag,DocItemNo, fileName))
    MetaValsDF <- do.call(rbind, MetaValsDF)
    TextwithsID <- TextwithsID[,colnames(TextwithsID)!=MetadataTag]

  }


#  function(TextwithsID,HeadwordVar,TargetDir)
    if(length(HeadwordVar)>1){
      compHead <- as.character(paste(HeadwordVar, collapse = "_"))
      TextwithsID[[sym(compHead)]] <- paste(TextwithsID[[HeadwordVar[1]]],TextwithsID[[HeadwordVar[2]]],sep="_")
      HeadwordVar <- compHead
    }
  TextwithsID[[HeadwordVar]][is.na(TextwithsID[[HeadwordVar]])] <- ""
  headwords <- unique(TextwithsID[[HeadwordVar]][TextwithsID[[HeadwordVar]]!=""])
  #print('line 217')
  print(as.numeric(Cores))
  if (Sys.info()[['sysname']]!="Windows"){
    headwordsIndex <- mclapply(headwords, function(x) TextwithsID$sID[TextwithsID[[HeadwordVar]]==x])

  }else{
     cl <- makeCluster(as.numeric(Cores))
    headwordsIndex <- parLapply(cl, headwords, function(x) TextwithsID$sID[TextwithsID[[HeadwordVar]]==x])
    stopCluster(cl)
  }

  names(headwordsIndex) <- headwords
  fileName <- gsub(".flextext","",fileName)

  saveRDS(headwordsIndex, paste0(TargetDir,"/CorpusData/HeadwordIndex_", fileName,"_",DocItemNo ,".rds"))
  TextLemmaFreqs <- as.data.frame(table(TextwithsID[[HeadwordVar]][TextwithsID[[HeadwordVar]]!=""]))
  colnames(TextLemmaFreqs)[1] <- HeadwordVar
  write_fst(TextLemmaFreqs, paste0(TargetDir,"/OutputFreqs/HeadwordFreqs_", fileName,"_",DocItemNo,".fst"))

  if(!is.null(MetadataTag) && !is.null(MetaValsDF) & nrow(MetaValsDF)>0){
    return(MetaValsDF) # once all corpus files are processed, rbinf the MetaValsDF and write it to file, putting CorpusName in filename, in case one uses multiple corpora, whcih would yield MetaDFs with different cols
  }
}


MakeCorpusTablesFromFlexAndReturnMeta <- function(HeadwordVar,MetadataTag, CorpusDir,  fileName, TargetDir="./data", csvTargetDir){

  # based on tested flextext files, available variables are: "txt" (= word form), "cf" (?lemma?), "gls" (equivalent in target lang), "msa" (?pos?), "morph" (morphological info)

  TextwithsIDs <- flextextToDF(CorpusDir, fileName, TargetDir, csvTargetDir)


  Meta <- lapply(seq_along(TextwithsIDs), function(i)  GetDocMetaAndFreqs(TextwithsIDs[i][[1]],HeadwordVar,MetadataTag,TargetDir,i, fileName))
  Meta <- do.call(rbind,Meta)
  #print(Meta)
  return(Meta)

}

ConvertFlexToCorpus <- function(HeadwordVar, CorpusDir,TargetDir="./data", csvTargetDir=NULL){


  if (Sys.info()[['sysname']]!="Windows"){
    mclapply(dir(CorpusDir), function(x) MakeCorpusTablesFromFlexAndReturnMeta(HeadwordVar,"doc" , CorpusDir, x, TargetDir, csvTargetDir))

  }else{
    cl <- makeCluster(as.numeric(Cores))

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

    Meta <- parLapply(cl, dir(CorpusDir), function(x) MakeCorpusTablesFromFlexAndReturnMeta(HeadwordVar,"doc" , CorpusDir, x, TargetDir, csvTargetDir))
    stopCluster(cl)
    return(Meta)
  }

}



