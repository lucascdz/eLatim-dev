
# functions to extract LL-LR kwywords from a lemma's kwics

GetLogDice <- function(Lemma,Collocate, LemmaFreq, CollocateFreq, CooccurenceFreq){

  LogDice<- 14+log(2*CooccurenceFreq/(LemmaFreq + CollocateFreq))
  #return(data.frame(Lemma,Collocate,LogDice))
  return(LogDice)
}


GetMI <- function(CoOccurenceFreq, NodeFreq, ColloFreq, CorpusSize){
  # log(O11/E11)
  # O11 co-occurrence Freq (node & collocate) # (example node = love collocate = affair)
  # E11 (Freq of node in whole Corpus * freq of collocate) / corpus size
  O11 <- CoOccurenceFreq
  C1 <-  ColloFreq
  R1 <- NodeFreq
  E11 <- (R1*C1)/CorpusSize
  MI <- log(O11/E11)
  return( MI)
}

LR_LLcalculator_DDL <- function(DF,HeadwordVar, SizeFocusCorpus, SizeRefcorpus, MinFreq=5, Stopwords){
  # DF must have Freq_FocusCorpus for SubCorpus/MainCorpus freqs and Freq_RefCorpus fro RefCorpus freqs
  DF <- na.omit(DF)

  DF <- DF[DF[[HeadwordVar]]!="",]
  DF <- DF[DF$TotalFreq > MinFreq & DF$Freq_FocusCorpus > 2,] # exclude  rare words
  DF <- DF[!DF[[HeadwordVar]] %in% Stopwords,]
  # set frequencies as.double to avoid `integer overflow` error with large integers
  DF$Freq_FocusCorpus <- as.double(DF$Freq_FocusCorpus)
  DF$Freq_RefCorpus <- as.double(DF$Freq_RefCorpus)

  DF$Freq_RefCorpus[DF$Freq_RefCorpus < 1] <- 0.1
  # uncomment the lines below if you want to add 1 to the Freqs to avoid zero division
  # the addtion however thus alter the results a bit, so use with caution
  # DF$Freq_FocusCorpus <- as.double(DF$Freq_FocusCorpus)+1 # add one to both Freqs to avoid having zeroes and resulting NaNs & Inf
  # DF$Freq_RefCorpus <- as.double(DF$Freq_RefCorpus)+1 # add one to both Freqs to avoid having zeroes and resulting NaNs & Inf

  eCorpus <- SizeFocusCorpus*(DF$Freq_FocusCorpus + DF$Freq_RefCorpus)/(SizeFocusCorpus+SizeRefcorpus)
  eRefcorpus <- SizeRefcorpus*(DF$Freq_FocusCorpus + DF$Freq_RefCorpus)/(SizeFocusCorpus+SizeRefcorpus)
  LogLikCorpusVSRefcorpus <- 2*((DF$Freq_FocusCorpus * log(DF$Freq_FocusCorpus /eCorpus)) + (DF$Freq_RefCorpus * log(DF$Freq_RefCorpus /eRefcorpus)))
  LogLikCorpusVSRefcorpus <- round(LogLikCorpusVSRefcorpus, digits=2)

  NORMfreqFocusCorpus <- (DF$Freq_FocusCorpus*10000)/SizeFocusCorpus
  NORMfreqRefCorpus <- (DF$Freq_RefCorpus*10000)/SizeRefcorpus

  LR <- log2(NORMfreqFocusCorpus/NORMfreqRefCorpus)

  DF_LL <- data.frame(DF,LogLikCorpusVSRefcorpus)
  DF_LL_LR <- data.frame(DF_LL, LR)
  DF_LL_LR <- cbind(DF_LL_LR,NORMfreqFocusCorpus , NORMfreqRefCorpus)
  return(DF_LL_LR)
}

GetKwicsLL_LR_DDL <- function(Kwics, Lem, CorpusFreqs, HeadwordVar, MinFreq=50, Stopwords= "", minLL=7, minLR=0.5, minMI=7,minLogDice=7, SortBy="LogLikCorpusVSRefcorpus", MaxKW=20, Cores=2){

  # Stopwords must be a character vector
  # SortBy is colname to sort by
# the first variable in CorpusFreqs must contain the lemmas, but can take any name
# the second variable in CorpusFreqs must contain the lemma's freqs, again can take any name

  # coerce thresholds args to NA in case they are string (e.g. "select")
  minLR <- as.numeric(minLR)
  minLL <- as.numeric(minLL)
  MinFreq <- as.numeric(MinFreq)
  minLogDice <- as.numeric(minLogDice)
  minMI <- as.numeric(minMI)

  colnames(CorpusFreqs)[1:2] <- c(HeadwordVar, "TotalFreq") # rename $lemma to HeadwordVar & change $Freq to TotalFreq for smoother join

  Kwics <- Kwics[str_detect(Kwics[[HeadwordVar]],"[[:lower:]]|[[:upper:]]"),]
  KwicslemmasFreqDF <- as.data.frame(table(Kwics[[HeadwordVar]][Kwics[[HeadwordVar]]!=Lem]))
  colnames(KwicslemmasFreqDF)[1] <- HeadwordVar

  # get ref corpus Freqs
  if(nrow(KwicslemmasFreqDF)>0){
    CorpusFreqs <- full_join(CorpusFreqs[,1:2], KwicslemmasFreqDF, by= HeadwordVar)
    CorpusFreqs[is.na(CorpusFreqs)] <- 0
    #head(CorpusFreqs)
    CorpusFreqs$Freq_RefCorpus <- CorpusFreqs$TotalFreq-CorpusFreqs$Freq
    colnames(CorpusFreqs)[3] <- "Freq_FocusCorpus"


    #get LL_LR kewywords

    LL_LRdf <- LR_LLcalculator_DDL(CorpusFreqs, HeadwordVar, sum(CorpusFreqs$Freq_FocusCorpus), sum(CorpusFreqs$Freq_RefCorpus), MinFreq, Stopwords )
    if(!is.null(LL_LRdf) && nrow(LL_LRdf)>0){
      if(!is.null(minMI) && !is.na(minMI) && length(minMI)>0){
        # add Mutual Information
        if (Sys.info()[['sysname']]!="Windows"){
        MItable <- mclapply(LL_LRdf$lemma, function(Collo) data.frame(lemma=Collo, MI= GetMI(
          CoOccurenceFreq = LL_LRdf$Freq_FocusCorpus[LL_LRdf$lemma==Collo],
          NodeFreq = CorpusFreqs$TotalFreq[CorpusFreqs$lemma==Lem],
          ColloFreq = CorpusFreqs$TotalFreq[CorpusFreqs$lemma==Collo],
          CorpusSize = sum(CorpusFreqs$TotalFreq) ), stringsAsFactors = F),mc.cores=Cores)
        }else{
          MItable <- lapply(LL_LRdf$lemma, function(Collo) data.frame(lemma=Collo, MI= GetMI(
            CoOccurenceFreq = LL_LRdf$Freq_FocusCorpus[LL_LRdf$lemma==Collo],
            NodeFreq = CorpusFreqs$TotalFreq[CorpusFreqs$lemma==Lem],
            ColloFreq = CorpusFreqs$TotalFreq[CorpusFreqs$lemma==Collo],
            CorpusSize = sum(CorpusFreqs$TotalFreq) ), stringsAsFactors = F))
        }

        MItable <- do.call(rbind, MItable)
        colnames(MItable)
       if(!is.null(MItable) && length(MItable)>0 && nrow(MItable)>0){
        LL_LRdf <- left_join(LL_LRdf, MItable, by= "lemma")
        LL_LRdf <- LL_LRdf[LL_LRdf$MI > minMI,]
       }else{
         LL_LRdf$MI <- 0
       }
      }

      # add LogDice
      if(!is.null(minLogDice) && !is.na(minLogDice) && length(minLogDice)>0){
        if(!is.null(LL_LRdf) && nrow(LL_LRdf)>0){
          if (Sys.info()[['sysname']]!="Windows"){
        LogDiceTable <- mclapply(LL_LRdf$lemma, function(Collo) data.frame(lemma=Collo, LogDice= GetLogDice(
          Lemma = Lem,
          Collocate = Collo,
          LemmaFreq = CorpusFreqs$TotalFreq[CorpusFreqs$lemma==Lem],
          CollocateFreq = CorpusFreqs$TotalFreq[CorpusFreqs$lemma==Collo],
          CooccurenceFreq = LL_LRdf$Freq_FocusCorpus[LL_LRdf$lemma==Collo]), stringsAsFactors = F),mc.cores=Cores)
        }else{
          LogDiceTable <- lapply(LL_LRdf$lemma, function(Collo) data.frame(lemma=Collo, LogDice= GetLogDice(
            Lemma = Lem,
            Collocate = Collo,
            LemmaFreq = CorpusFreqs$TotalFreq[CorpusFreqs$lemma==Lem],
            CollocateFreq = CorpusFreqs$TotalFreq[CorpusFreqs$lemma==Collo],
            CooccurenceFreq = LL_LRdf$Freq_FocusCorpus[LL_LRdf$lemma==Collo]), stringsAsFactors = F))
        }
        LogDiceTable <- do.call(rbind, LogDiceTable)
        if(!is.null(LogDiceTable) && length(LogDiceTable)>0 && nrow(LogDiceTable)>0){
        LL_LRdf <- left_join(LL_LRdf, LogDiceTable, by="lemma")
        LL_LRdf <- LL_LRdf[LL_LRdf$LogDice > minLogDice,]
        }else{
          LL_LRdf$LogDice <- 0
        }
        }
      }

      if(!is.null(minLL) && !is.na(minLL) && length(minLL)>0){
        LL_LRdf <- LL_LRdf[LL_LRdf$LogLikCorpusVSRefcorpus > minLL,]
      }
      if(!is.null(minLR) && !is.na(minLR)  && length(minLR)>0){
        LL_LRdf <- LL_LRdf[LL_LRdf$LR > minLR,]
      }

      # sort & trim DF to top KW
      if(is.null(SortBy)){
        SortBy <- "LogLikCorpusVSRefcorpus"
      }
      if(!SortBy %in% colnames(LL_LRdf)){
        SortBy <- "LogLikCorpusVSRefcorpus"
        # print("your sort-by parameter is not in data: sorting by loglikelihood")
      }
      LL_LRDF <- LL_LRdf[order(LL_LRdf[[SortBy]],decreasing=T),]

      if(nrow(LL_LRDF)>as.numeric(MaxKW)){
        #print(paste("nrow(LL_LRDF): ", nrow(LL_LRDF)))
        LL_LRDF <- LL_LRDF[1:as.numeric(MaxKW),]
      }

      return(LL_LRDF)
    }
  }
}


# process each headowrd individually to create outputs while all headwords are being processed
# this is to show progress, avoid loosing data if process is interrupted
# and create more digestible individual file for manual editing

GetColloByLemma <- function(Lem, HeadFreqs, window=5, HeadwordVar, Stopwords="", MinFreq=10, minLL=10, minLR=1, minMI=0,minLogDice=9,SortBy=NULL,MaxKW=20,Cores){
  # writes one file per headword
  # process each headowrd individually to create outputs while all headwords are being processed
  # this is to show progress, avoid loosing data if process is interrupted
  # and create more digestible individual file for manual editing
  kwics <- GetAllLemmaSents(Lem,"./data/CorpusDocs", HeadwordVar=HeadwordVar, Window=window,Cores=Cores)
  if(!is.null(kwics) && nrow(kwics)>0){
    KeyWDF <- GetKwicsLL_LR_DDL(kwics,Lem, HeadFreqs, HeadwordVar, MinFreq= MinFreq, Stopwords= Stopwords, minLL= minLL, minLR= minLR, minMI= minMI, minLogDice=minLogDice, SortBy=SortBy, MaxKW=MaxKW,  Cores= Cores)
    if(!is.null(KeyWDF) && nrow(KeyWDF)>0){
      colnames(KeyWDF)[1] <- "collocate" # changes from "lemma.1"
      colnames(KeyWDF)[which(colnames(KeyWDF)=="LogLikCorpusVSRefcorpus")] <- "LogL" # changes from "LogLikCorpusVSRefcorpus"
      KeyWDF <- KeyWDF[,!colnames(KeyWDF) %in% c("NORMfreqFocusCorpus","NORMfreqRefCorpus", "Freq_RefCorpus","lemma.1","Freq_FocusCorpus","TotalFreq")]
      KeyWDF$lemma <- Lem
      KeyWDF <- KeyWDF[,c(length(KeyWDF),1:(length(KeyWDF)-1))] # reorder columns to: "lemma","collocate","LogL","LR","MI","LogDice","CoFreq","TotalFreq"
    }
  }
  parameters <- paste0(Lem ,"_MinFreq",MinFreq,"minLL",minLL,"minLR",minLR,"minMI",minMI,"minLogDice",minLogDice)

if(!file.exists('./data/Outputs/Collocates')){
  if (Sys.info()[['sysname']]!="Windows"){

    system("mkdir ./data/Outputs/Collocates")
  }else{
    shell("mkdir .\\data\\Outputs\\Collocates")
  }
}
if(!is.null(KeyWDF) && nrow(KeyWDF)>0){
  fwrite(KeyWDF, paste0("./data/Outputs/Collocates/CollocateCandidates_",parameters ,".csv"))
  return(KeyWDF)
}
}

GetColloForAllLemmaAtOnce <- function(HeadwordVec, HeadFreqs, window=5, HeadwordVar, Stopwords="", MinFreq=10, minLL=10, minLR=1, minMI=0,minLogDice=9,SortBy=NULL,MaxKW=20,Cores){
  # processes all headwords together and stores their collocates/top keywords into a single DF
  # does NOT write the result to file!
  if (Sys.info()[['sysname']]!="Windows"){
  allLemmataSents <- mclapply(HeadwordVec, function(x) GetAllLemmaSents(x,"./data/CorpusDocs", HeadwordVar=HeadwordVar, Window=force(window),Cores=Cores),mc.cores=Cores)
  }else{
  allLemmataSents <- lapply(HeadwordVec, function(x) GetAllLemmaSents(x,"./data/CorpusDocs", HeadwordVar=HeadwordVar, Window=force(window),Cores=Cores))
  }
  names(allLemmataSents) <- HeadwordVec
  allLemmataSents <- allLemmataSents[!sapply(allLemmataSents, is.null)]
  if(!is.null(allLemmataSents) && length(allLemmataSents)>0){
    KeyW <- lapply(seq_along(allLemmataSents), function(i) GetKwicsLL_LR_DDL(allLemmataSents[i][[1]],names(allLemmataSents[i]), HeadFreqs, HeadwordVar, MinFreq=force(MinFreq), Stopwords=force(Stopwords), minLL=force(minLL), minLR=force(minLR), minMI=force(minMI), minLogDice=force(minLogDice), SortBy=force(SortBy), MaxKW=force(MaxKW),  Cores = Cores))
    names(KeyW) <- names(allLemmataSents)
    KeyW <- KeyW[!sapply(KeyW, is.null)]
    KeyW <- Filter(function(x) nrow(x)>0, KeyW)
    if(!is.null(KeyW) && length(KeyW)>0){
      KeyWDF <- lapply(seq_along(KeyW), function(i) if(nrow(KeyW[i][[1]])>0){data.frame(lemma=names(KeyW)[i],KeyW[i][[1]])})
      KeyWDF <- do.call(rbind, KeyWDF)
      #colnames(KeyWDF)
      colnames(KeyWDF)[1:2] <- c("lemma","collocate") # changes from "lemma.1"
      colnames(KeyWDF)[which(colnames(KeyWDF)=="LogLikCorpusVSRefcorpus")] <- "LogL" # changes from "LogLikCorpusVSRefcorpus"
      KeyWDF <- KeyWDF[,!colnames(KeyWDF) %in% c("NORMfreqFocusCorpus","NORMfreqRefCorpus", "Freq_RefCorpus","lemma.1","Freq_FocusCorpus","TotalFreq")]
      #KeyWDF <- KeyWDF[,c(length(KeyWDF),1:(length(KeyWDF)-1))] # reorder columns to: "lemma","collocate","LogL","LR","MI","LogDice","CoFreq","TotalFreq"

     return(KeyWDF)
    }
  }
}



