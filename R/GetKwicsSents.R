# get kwics for collocation stats (with numeric Window arg) + sents for examples (with Window=NULL)

GetKwic <- function(Sent, Lem, Window, HeadwordVar){
  Sent$RowNo <- 1:nrow(Sent)
  LemIndex <- Sent$RowNo[Sent[[HeadwordVar]]==Lem]
  Start <- ifelse((LemIndex-Window) %in% Sent$RowNo, (LemIndex-Window), 0)
  End <- ifelse((LemIndex+Window) %in% Sent$RowNo, (LemIndex+Window), nrow(Sent))

  Kwic <- Sent[Start:End,]
  Kwic <- select(Kwic, -RowNo)
  Kwic[is.na(Kwic)] <- ""
  Kwic <- Kwic[Kwic[[HeadwordVar]]!="",] # remove tags and non-lexical rows

  return(Kwic)
}

GetLemmaSent <- function(Lem, CorpusDocFilename, Window=NULL, HeadwordVar){

  # if Window=NULL returns a DF with the full corpus sentneces in which the lenma Lem occurs + one col with the source text
  # if Window is a numberr it returns a Kwic with window-number of words to left and right in the SAME sentnece, if sentnece ends before the window, the words retrieved are less than window.
  if(!is.null(Window) && !is.na(Window) && Window!=""){
  Window <- as.numeric(Window)
  }

  FileIndex <- readRDS(paste0("./data/CorpusData/HeadwordIndex_",gsub("fst$","rds", CorpusDocFilename)))
  LemIndex <- FileIndex[[Lem]]

  if(!is.null(LemIndex) && length(LemIndex)>0){


    SentIndex <- read.fst(paste0("./data/CorpusData/sIDindex_",CorpusDocFilename))
    SentIndex <- SentIndex[SentIndex$sID %in% LemIndex,]

    Sents <- lapply(SentIndex$sID, function(x) read.fst(paste0("./data/CorpusDocs/",CorpusDocFilename), from=SentIndex$Start[SentIndex$sID==x], to=SentIndex$End[SentIndex$sID==x]  ))


    if(!is.null(Window) && !is.na(Window)){

      kwics <- lapply(Sents, function(x) GetKwic(x, Lem, Window, HeadwordVar))
      kwics <- do.call(rbind,kwics)


      return(kwics)


    }else{

      Sents <- do.call(rbind, Sents)
      Sents[is.na(Sents)] <- ""

      Sents <- Sents[as.character(Sents[[HeadwordVar]])!="",] # remove tags and non-lexical rows

      Sents$source <- gsub(".fst$","",CorpusDocFilename)
      return(Sents)
    }

    # return list of Sents containing Lem
  }
}
GetAllLemmaSents <- function(Lem, CorpusDocsDir="./data/CorpusDocs",HeadwordVar,Window=NULL, Cores){

  if (Sys.info()[['sysname']]!="Windows"){
  AllLemSents <- mclapply(dir(CorpusDocsDir), function(x) GetLemmaSent(Lem,x,force(Window),HeadwordVar), mc.cores = Cores)
  }else{
  AllLemSents <- lapply(dir(CorpusDocsDir), function(x) GetLemmaSent(Lem,x,Window,HeadwordVar))
  }
  AllLemSents <- do.call(rbind, AllLemSents)
  return(AllLemSents)
}

