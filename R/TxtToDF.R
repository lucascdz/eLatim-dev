# get corpus tables and freqs from horizontal txt files

if (! "udpipe" %in% rownames(installed.packages()) ) {
  install.packages("udpipe",repos='http://cran.us.r-project.org', type='source')
}
if (! "textstem" %in% rownames(installed.packages()) ) {
  install.packages("textstem",repos='http://cran.us.r-project.org',dependencies = T, type='source')
}
if (! "tidytext" %in% rownames(installed.packages()) ) {
  install.packages("tidytext",repos='http://cran.us.r-project.org',dependencies = T, type='source')
}

require(tidytext)
require(textstem)
require(udpipe)


processWithUdpipe <- function(Text,udmodel){
  Text <- udpipe_annotate(udmodel, x = Text)
  Text <- as.data.frame(Text)
  Text <- Text[,3:13]
  colnames(Text)[1] <- "sID"
  return(Text)
}

CreateTables <- function(CorpusDF, HeadwordVar, filename, TargetDir){

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
    #clusterExport(cl, c('TextwithsID', 'HeadwordVar', 'headwords'))
    headwordsIndex <- parLapply(cl, headwords, function(x) CorpusDF$sID[CorpusDF[[HeadwordVar]]==x])
    stopCluster(cl)
  }



  names(headwordsIndex) <- headwords

  saveRDS(headwordsIndex, paste0(TargetDir,"/CorpusData/HeadwordIndex_",gsub(".txt$","",filename),".rds"))
  TextLemmaFreqs <- as.data.frame(table(CorpusDF[[HeadwordVar]][CorpusDF[[HeadwordVar]]!=""]))
  colnames(TextLemmaFreqs)[1] <- HeadwordVar
  write_fst(TextLemmaFreqs, paste0(TargetDir,"/OutputFreqs/HeadwordFreqs_",gsub(".txt$","",filename),".fst"))

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
  write.fst(sIdIndexDF, paste0("./data/CorpusData/", "sIDindex_",gsub(".txt$","",filename),".fst" ))
  CorpusDF <- CorpusDF[,colnames(CorpusDF)!="RowID"]

  write_fst(CorpusDF, paste0(TargetDir,"/CorpusDocs/",gsub(".txt$","",filename),".fst"))

}

processWithTidytext <- function(Text,filename, SentEndMarker="([.!?])"){
  Text <- gsub(SentEndMarker, "\\1 endofsent ", Text, fixed=T)
  Text <- tibble(text=Text, book=gsub("\\.txt$","", filename))
  TextWord <- unnest_tokens(Text, word, text)
  TextLem <- lemmatize_words(TextWord$word)
  TextVert <- as.data.frame(cbind(word=TextWord$word, lemma=TextLem))
  TextVert <- AddsIDCol(TextVert, "endofsent", "word", filename)
  TextVert$word[TextVert$word=="endofsent"] <- TextVert$word[TextVert$word!="endofsent"]
  return(TextVert)
}

txtToDF <- function(udmodel=NULL,HeadwordVar, CorpusDir, filename, SentEndMarker=".", TargetDir, conlluTargetDir=NULL){
  Text <- readtext(paste0(CorpusDir,"/",filename))
  Text <- Text$text
  if(!is.null(udmodel)){
    Text <- processWithUdpipe(Text,udmodel)
    if(!is.null(conlluTargetDir)){
      fwrite(Text, paste0(conlluTargetDir,"/",filename,".csv"))
    }
  }else{
    Text <- processWithTidytext(Text,filename, SentEndMarker)
    if(!is.null(conlluTargetDir)){
      fwrite(Text, paste0(conlluTargetDir,"/",filename,".csv"))
    }
  }

  CreateTables(Text, HeadwordVar,filename,TargetDir)
}

ConvertTxtCorpus <- function(Language, CorpusDir, SentEndMarker=".", TargetDir="./data", conlluTargetDir){
  # the Language argument must be lowercase name of language, e.g. english

  model <- tryCatch(
    {
      udpipe_download_model(language = Language)
    },
    error=function(e){
      NULL
    }
  )

  if(!is.null(model)){
    udmodel <- udpipe_load_model(file = model$file_model)
  }else{
    udmodel <- NULL
  }

  lapply(dir(CorpusDir), function(x) txtToDF(udmodel, HeadwordVar="lemma", CorpusDir, x, SentEndMarker, TargetDir,conlluTargetDir))

}



