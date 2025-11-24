
# 1059___udAnavarga
#  GdexRulesTemplate <- data.frame(variable="", value="", operation="", penalty_bonus="", applyToConverse="")
#write.csv(GdexRulesTemplate, "~/Desktop/GdexRulesTemplate.csv", row.names = F)

# sort sents by gdex
# optimization trials


GetSENTS <- function(CorpusDocsDir="./data/CorpusDocs", HeadwordVar, LEM, Cores){

  Sents <- GetAllLemmaSents(LEM, CorpusDocsDir , HeadwordVar , Window=NULL, Cores)
  Sents$UID <- paste(Sents$sID,Sents$source, sep="___")

  return(Sents)
}
GetDFfromFile <- function(Dir, filenameString){
  # THE FILEPATH FOR COLLOCATION SHOULD CHANGE IF Collocations.R SWITCHES FROM GetColloForAllLemmaAtOnce() TO GetColloByLemma
  File <- dir(Dir)[str_detect(dir(Dir), filenameString)]
  if(!is.null(File)  && length(File)==1 && !is.na(File)){
    DF <- read.csv(paste0(Dir,"/" ,File), stringsAsFactors = F)
  }else{
    DF <- NULL
  }
  return(DF)
}

FastSorter <- function(CorpusDocsDir="./data/CorpusDocs", HeadwordVar, LEM, GdexRulesDF,CollocatesDF,collocateBoost, LenRange,LongerPen,ShorterPen, Cores){

  LemSents <- GetSENTS(CorpusDocsDir, HeadwordVar, LEM, Cores)
  LemSents$Score <- 50

  if(!is.null(CollocatesDF) && !is.null(collocateBoost) && collocateBoost > 0){
    # award collocateBoost
    CumColloUID <- unique(LemSents$UID[LemSents[[HeadwordVar]] %in% CollocatesDF$collocate[CollocatesDF$lemma==LEM]])
    LemSents$Score[LemSents$UID %in% CumColloUID] <- LemSents$Score[LemSents$UID %in% CumColloUID] + collocateBoost
  }


  SentLens <- as.data.frame(table(LemSents$UID[LemSents[[HeadwordVar]]!=""]))
  colnames(SentLens) <- c("UID","Len")
  SentLens$penaltyLong <- 0
  SentLens$penaltyLong[SentLens$Len > max(LenRange)] <-  (SentLens$Len[SentLens$Len > max(LenRange)]- max(LenRange)) * LongerPen
  if(!is.null(SentLens) && nrow(SentLens[SentLens$penaltyLong > 0,])>0){
    # colnames(penaltyLong) <- c("UID","penaltyLong")
    LemSents <- left_join(LemSents, SentLens[,colnames(SentLens) %in% c("UID", "penaltyLong")], by ="UID")
    LemSents$penaltyLong[is.na(LemSents$penaltyLong)] <- 0
    LemSents$Score <- LemSents$Score -  LemSents$penaltyLong
  }else{
    LemSents$penaltyLong <- 0
  }

SentLens$penaltyShort <- 0
SentLens$penaltyShort[SentLens$Len < min(LenRange)] <-  (SentLens$Len[SentLens$Len < min(LenRange)]- min(LenRange)) * ShorterPen
if(!is.null(SentLens) && nrow(SentLens[SentLens$penaltyShort > 0,])>0){
  # colnames(penaltyShort) <- c("UID","penaltyShort")
   LemSents <- left_join(LemSents, SentLens[,colnames(SentLens) %in% c("UID", "penaltyShort")], by ="UID")
   LemSents$penaltyShort[is.na(LemSents$penaltyShort)] <- 0
  LemSents$Score <- LemSents$Score -  LemSents$penaltyShort
}else{
  LemSents$penaltyShort <- 0
}

  # apply GdexRules
  if(!is.null(GdexRulesDF) && nrow(GdexRulesDF)>0){

    for(Row in 1:nrow(GdexRulesDF)){
      Val <-   GdexRulesDF[Row,]$value
      if(!is.null(Val) && nchar(Val)>0){
        if(str_detect(Val, "\\.csv \\d+$")){
          File <- read.csv(gsub("^(.*?\\.csv) (\\d+)$" ,"\\1",Val), stringsAsFactors = F)
          Val <- File[,as.numeric(gsub("^(.*?\\.csv) (\\d+)$" ,"\\2",Val))]
        }
        Var <- GdexRulesDF[Row,]$variable
        Val <- intersect(LemSents[[Var]], Val)
        if(length(Val) >0 ){
          DFforxtabs <- LemSents[, colnames(LemSents) %in% c("UID", Var)]
          colnames(DFforxtabs)[colnames(DFforxtabs)!="UID"] <- "VAR"

          if(length(Val) ==1 ){
            PenBonDF <- data.frame(xtabs(~ UID + VAR, DFforxtabs[DFforxtabs$VAR %in% Val,] ))
          }else if(length(Val) > 1){

            PenBonDF <- DFforxtabs[DFforxtabs$VAR %in% Val,]
            PenBonDF <- data.frame(table(PenBonDF$UID))
            colnames(PenBonDF)[1] <- "UID"
          }

          PenBonDF <- PenBonDF[PenBonDF$Freq>0,]
          colNam <- paste0("penaltyBonus_", Row)
          PenBonDF[[colNam]] <- PenBonDF$Freq * as.numeric(GdexRulesDF[Row,]$penalty_bonus)
          LemSents <- left_join(LemSents, PenBonDF[ , colnames(PenBonDF) %in% c("UID",colNam)], by ="UID")
          LemSents[[colNam]][is.na(LemSents[[colNam]])] <- 0
          LemSents$Score <- LemSents$Score + LemSents[[colNam]]
        }
      }
    }
  }
LemSents$filename<- gsub("^\\d+___(.*?)$","\\1" ,LemSents$UID)
  return(LemSents)
}

SampleSentsByVal <-  function(SentsScoredDF, Val, Var = HeadwordVar, samplesize=3){
  MatchingSents <-  SentsScoredDF[SentsScoredDF[[Var]] == Val,]
  if(nrow(MatchingSents) > 0){
  UIDs <- unique(MatchingSents$UID[MatchingSents$Score==max(MatchingSents$Score)])
  if(length(UIDs)>=samplesize){
    UIDs[1:samplesize]
  }else{
    UIDs[1:length(UIDs)]
  }
  }
}

RemoveSimilarSents <- function(SentsDF,simThreshold=0.1){

  toRemove <- c()
  toKeep <- c()
  for(i in 1:nrow(SentsDF)){
    x <- SentsDF$ID[i]
    if(!x %in% toRemove){
      sim <- SentsDF$ID[ which(stringdist::stringdist(SentsDF$Sent[i], SentsDF$Sent, method = "jw") < simThreshold)]
      toRemove <- c(toRemove,sim)
      toKeep <- c(toKeep,x)
    }
  }

  return(SentsDF[SentsDF$ID %in% toKeep,])
}



SamplerPerVarCollo <- function(LEM, SentsScoredDFsub, CollocatesDF, HeadwordVar, Var, subSampleSize, Cores){
  if(nrow(SentsScoredDFsub)>subSampleSize){
    if(subSampleSize > length(unique(SentsScoredDFsub$UID))){
      subSampleSize <- length(unique(SentsScoredDFsub$UID))
    }
    SampledUIDs <- c()
    sampledFilenames <-c()

    # sample by collocate

    if(!is.null(CollocatesDF) && nrow(CollocatesDF)>0){
      Collos <- CollocatesDF$collocate[CollocatesDF$lemma==LEM]
      sampledCollo <- c()
      while(length(SampledUIDs) <= subSampleSize){
        sam <- SentsScoredDFsub[!SentsScoredDFsub$UID %in% SampledUIDs & !SentsScoredDFsub$filename %in% sampledFilenames & SentsScoredDFsub[[HeadwordVar]] %in% Collos[!Collos %in% sampledCollo],]
        maxScoreUID <- sam$UID[which.max(sam$Score)]
        sam <- sam[sam$UID==maxScoreUID,]
         if(nrow(sam)==0){
          sam <- SentsScoredDFsub[!SentsScoredDFsub$UID %in% SampledUIDs & SentsScoredDFsub[[HeadwordVar]] %in% Collos[!Collos %in% sampledCollo],]
          maxScoreUID <- sam$UID[which.max(sam$Score)]
          sam <- sam[sam$UID==maxScoreUID,]
        }
        if(nrow(sam)==0){
          SampledUIDs <- c(SampledUIDs,"")
          sampledFilenames <-c(sampledFilenames, "")
          sampledCollo <- c(sampledCollo,"")
        }else{
          SampledUIDs <- c(SampledUIDs, sam$UID[1])
          sampledFilenames <-c(sampledFilenames, sam$filename[1])
          col <- sam[[HeadwordVar]][sam[[HeadwordVar]] %in% Collos[!Collos %in% sampledCollo]]
          sampledCollo <- c(sampledCollo,col[1])
        }
      }
    }else{
      SampledUIDs <- c()
      sampledFilenames <-c()
    }

    SampledUIDs <- SampledUIDs[SampledUIDs!=""]
    sampledFilenames <- sampledFilenames[sampledFilenames!=""]
    # sample by Var or continue sampling till you reach desired sample size

    if(!is.null(Var) && Var %in% colnames(SentsScoredDFsub)){
      vars <- unique(SentsScoredDFsub[[Var]][SentsScoredDFsub[[HeadwordVar]]==LEM])
      sampledVars <- c()
    }else{
      sampledVars <- NULL
    }
    while(length(SampledUIDs) < subSampleSize){
      if(!is.null(sampledVars)){
        sam <- SentsScoredDFsub[!SentsScoredDFsub$UID %in% SampledUIDs & !SentsScoredDFsub$filename %in% sampledFilenames & !SentsScoredDFsub[[Var]][SentsScoredDFsub[[HeadwordVar]]==LEM] %in% sampledVars,]
        maxScoreUID <- sam$UID[which.max(sam$Score)]
        sam <- sam[sam$UID==maxScoreUID,]
      }else{
        sam <- SentsScoredDFsub[!SentsScoredDFsub$UID %in% SampledUIDs & !SentsScoredDFsub$filename %in% sampledFilenames,]
        maxScoreUID <- sam$UID[which.max(sam$Score)]
        sam <- sam[sam$UID==maxScoreUID,]
      }
      if(nrow(sam)==0){
        sam <- SentsScoredDFsub[!SentsScoredDFsub$UID %in% SampledUIDs,]
        maxScoreUID <- sam$UID[which.max(sam$Score)]
        sam <- sam[sam$UID==maxScoreUID,]
      }
      if(nrow(sam)==0){
        SampledUIDs <- c(SampledUIDs,"")
        sampledFilenames <-c(sampledFilenames, "")
        if(!is.null(sampledVars)){
          sampledVars <- c( sampledVars,"")
        }
      }else{
        SampledUIDs <- c(SampledUIDs, sam$UID[1])
        sampledFilenames <-c(sampledFilenames, sam$filename[1])
      }
      if(!is.null(sampledVars)){
        sampledVars <- sam[[Var]][sam[[HeadwordVar]]==LEM][1]
      }
    }

    return(SentsScoredDFsub[SentsScoredDFsub$UID %in% SampledUIDs,])
  }else{
    return(SentsScoredDFsub)
  }
}


Sampler <- function(LEM, SentsScoredDF, CollocatesDF, HeadwordVar, wordFormVar, Var, SampleSize, MetaDF=NULL, MetaVar=NULL, MinScore, Cores){
  if(!is.null(MinScore) && is.numeric(MinScore)){
    SentsScoredDF <- SentsScoredDF[SentsScoredDF$Score > MinScore,]
  }
  if(length(unique(SentsScoredDF$UID))>SampleSize){
  # if MetaVar is used, a numebr of sents = SampleSize will be sampled PER EACH MetaVar.
  if(!is.null(MetaDF) && !is.null(MetaVar)){
    # create metavar Col in SentsScoredDF
    MetaVals <- unique(MetaDF[[MetaVar]])
    MetDF <- as.data.frame(MetaDF)
    MetDF <- MetDF[,which(colnames(MetaDF) %in% c("filename", MetaVar))]
    MetDF <- unique(MetDF)
    SentsScoredByMeta <- left_join(SentsScoredDF, MetDF, by="filename" )
    SentsScoredByMetaSplit <- split(SentsScoredByMeta, SentsScoredByMeta[[MetaVar]])

    sampledsentsDF <- lapply(SentsScoredByMetaSplit, function(x) SamplerPerVarCollo(LEM, SentsScoredDFsub=x, CollocatesDF, HeadwordVar, Var, subSampleSize=ceiling(SampleSize/length(SentsScoredByMetaSplit)), Cores))
    sampledsentsDF <- do.call(rbind, sampledsentsDF)

    count <-0
    while(length(unique(sampledsentsDF$UID)) < SampleSize){
      if(  count < 10){
      SentsScoredByMeta <- SentsScoredByMeta[!SentsScoredByMeta$UID %in% sampledsentsDF$UID,]
      SentsScoredByMetaSplit <- split(SentsScoredByMeta, SentsScoredByMeta[[MetaVar]])
      #similarSentsUIDs <- c()
      #newSampled <- lapply(SentsScoredByMetaSplit, function(x) SamplerPerVarCollo(LEM, x[!x$UID %in% c(sampledsentsDF$UID,similarSentsUIDs),], CollocatesDF, HeadwordVar, Var, ceiling(SampleSize/length(unique(MetaVals))), Cores))
      newSampled <- lapply(SentsScoredByMetaSplit, function(x) SamplerPerVarCollo(LEM, SentsScoredDFsub=x, CollocatesDF=CollocatesDF[CollocatesDF$lemma==LEM & !CollocatesDF$collocate %in% sampledsentsDF[[HeadwordVar]],], HeadwordVar, Var, subSampleSize=ceiling(SampleSize/length(SentsScoredByMetaSplit)), Cores))
      newSampled <- do.call(rbind, newSampled)
      sampledsentsDF <- rbind(sampledsentsDF,newSampled)
      count <-  count+1
      }else{
        newSampledUIDs <- sample(setdiff(SentsScoredByMeta$UID, unique(sampledsentsDF$UID)) , SampleSize-length(unique(sampledsentsDF$UID))  )
        newSampled <- SentsScoredByMeta[SentsScoredByMeta$UID %in% newSampledUIDs,]
        sampledsentsDF <- rbind(sampledsentsDF,newSampled)
      }
     # UIDS <- unique(sampledsentsDF$UID)
     # sampledsentsDF <- RemoveSimilarSents(sampledsentsDF,simThreshold=0.75,HeadwordVar,Cores )
     #similarSentsUIDs <- c(similarSentsUIDs, setdiff(UIDS, sampledsentsDF$UID))
    }

  }else{
    sampledsentsDF <- SamplerPerVarCollo(LEM, SentsScoredDF, CollocatesDF, HeadwordVar, Var, SampleSize, Cores)
  }

  # remove similar sents
    #sampledsentsDF <- RemoveSimilarSents(sampledsentsDF,simThreshold=0.90,HeadwordVar,Cores )
  # # add bold tag around lemma
  # sampledsentsDF[[wordFormVar]][sampledsentsDF[[HeadwordVar]]==LEM] <- paste0("<b>",sampledsentsDF[[wordFormVar]][sampledsentsDF[[HeadwordVar]]==LEM] ,"</b>")


  # if(length(unique(sampledsentsDF$UID)) > SampleSize){
  #   if(length(sampledsentsDF$UID[sampledsentsDF$Score==max(sampledsentsDF$Score)]) >= SampleSize){
  #     # sample from max scoring sents
  #     sampledsentsDF <- sampledsentsDF[sampledsentsDF$UID %in% sample(sampledsentsDF$UID[sampledsentsDF$Score==max(sampledsentsDF$Score)], SampleSize),]
  #   }else{
  #     # take top-scoring sents
  #     sampledsentsDF <- sampledsentsDF[sampledsentsDF$UID %in% unique(sampledsentsDF$UID)[1:SampleSize],]
  #   }
  # }

  return(sampledsentsDF)
  }else{
    return(SentsScoredDF)
  }
}

SortAndSample <- function(CorpusDocsDir="./data/CorpusDocs", HeadFreqs, HeadwordVar, wordFormVar, LEM, GdexRulesDF,CollocatesDF,collocateBoost, LenRange, LongerPen, ShorterPen, Var, SampleSize, MetaDF=NULL, MetaVar=NULL, MinScore=NULL, AdditionalVars="", Cores){

  if(nrow(HeadFreqs[HeadFreqs$lemma==LEM,])==1){
    SentsScoredDF  <- FastSorter(CorpusDocsDir, HeadwordVar, LEM, GdexRulesDF,CollocatesDF,collocateBoost, LenRange,LongerPen,ShorterPen, Cores)

    sampledSentsDF <- Sampler(LEM, SentsScoredDF, CollocatesDF, HeadwordVar, wordFormVar, Var, SampleSize, MetaDF, MetaVar, MinScore, Cores)
    if(!is.null(sampledSentsDF) && nrow(sampledSentsDF)>0){
    # add bold tag around lemma
    sampledSentsDF[[wordFormVar]][sampledSentsDF[[HeadwordVar]]==LEM] <- paste0("<b>",sampledSentsDF[[wordFormVar]][sampledSentsDF[[HeadwordVar]]==LEM] ,"</b>")

if(length(AdditionalVars)==0){
    sampledSentsDF <- sampledSentsDF[,colnames(sampledSentsDF) %in% c(wordFormVar, "UID","Score",colnames(sampledSentsDF)[str_detect(colnames(sampledSentsDF),"penalty")])]
}else{
    sampledSentsDF <- sampledSentsDF[,colnames(sampledSentsDF) %in% c(wordFormVar, "UID","Score",colnames(sampledSentsDF)[str_detect(colnames(sampledSentsDF),"penalty")],AdditionalVars)]
}

    return(sampledSentsDF[order(sampledSentsDF$Score, decreasing = T),])
    }else{
  print("no suitable examples for this lemma")
}
  }else{
    print("no single headword corresponding to selection found in ./data/Outputs/HeadwordFreqs.csv")
  }
}


CreateHorizontalGdexSentDF <- function(SampledSentsDF,wordFormVar, AdditionalVars=""){
  SampledSentsDF <- as.data.frame(SampledSentsDF) # convert from data.table for simpler subsetting
  #colVec <- c(colnames(SampledSentsDF)[str_detect(colnames(SampledSentsDF), "penalty")])
  if("lemma" %in% colnames(SampledSentsDF)){
    SampledSentsDF$UID <- paste(SampledSentsDF$lemma, SampledSentsDF$UID, sep="%%%")
  }

  SampledSentsSplit <- split(SampledSentsDF, SampledSentsDF$UID)

   if(length(AdditionalVars) > 0 && nchar(AdditionalVars)>0){
    SampledSentsDFs <- lapply(SampledSentsSplit, function(x) MakeHorizontalSentDFWithAddVars(wordFormVar, x, AdditionalVars))
  }else{
    SampledSentsDFs <- lapply(SampledSentsSplit, function(x) data.frame(ID = unique(x$UID), Sent= paste(x[[wordFormVar]], collapse = " "), lemma=unique(x$lemma),
                                                                        Score=unique(x$Score) , x[1,colnames(x)[str_detect(colnames(x), "penalty")]]))
  }

  SampledSentsDFs <- do.call(rbind, SampledSentsDFs)
  if("lemma" %in% colnames(SampledSentsDF)){
    SampledSentsDFs$lemma <- gsub("^(.*?)%%%(.*?)$", "\\1", SampledSentsDFs$ID)
    SampledSentsDFs$ID <- gsub("^(.*?)%%%(.*?)$", "\\2", SampledSentsDFs$ID)
  }
  return(SampledSentsDFs[order(SampledSentsDFs$Score, decreasing=T),])
}


MakeHorizontalSentDFWithAddVars <- function(wordFormVar, DF, AdditionalVars){
  # DF must be a df created with SorterFucntions' SortAndSample()
  LEMindeDF <- which(str_detect(DF[[wordFormVar]],"^<b>.*?</b>$"))[1]
  #AddVarsDF <- lapply(AdditionalVars, function(y) data.frame(var=DF[[y]][LEMindeDF]))

  AddVarsDF <- lapply(AdditionalVars, function(y) if(y!="DEPREL"){data.frame(var=DF[[y]][LEMindeDF])}else{getDeprel(DF,y,LEMindeDF)})
  AddVarsDF <- do.call(cbind, AddVarsDF)
  colnames(AddVarsDF) <- AdditionalVars
  SentDF <- data.frame(ID = unique(DF$UID), Sent= paste(DF[[wordFormVar]], collapse = " "), lemma=unique(DF$lemma),
                       Score=unique(DF$Score) , DF[1,colnames(DF)[str_detect(colnames(DF), "penalty")]], AddVarsDF)

  return(SentDF)
}

getDeprel <- function(DF,Var,LEMindeDF){
 # for use in MakeHorizontalSentDFWithAddVars()

  lemDep <- DF[[Var]][LEMindeDF]
  if(lemDep!="root"){
    deprel <- DF[["DEPREL"]][LEMindeDF]
    verb <- "is"
  }else{
    lemID <- DF[["ID"]][LEMindeDF]
    deprel <- DF[["DEPREL"]][which(DF[["HEAD"]]==lemID)]
    verb <- "governs"
  }
  if(length(deprel)==0){
    return(data.frame("", complPattern=""))
  }else if(length(deprel)==1){
    complPattern <- paste(verb, deprel , "solo")
  }else if(length(deprel) > 1){
    # deprel only takes the first DEPREL, but complPattern lists all
    complPattern <- paste(verb, paste(sort(deprel), collapse="+" ))
    deprel <- deprel[1]
  }
  return(data.frame(paste(verb, deprel), complPattern))
}




# FOR TESTING on FullBuddh:
 # MetaDF <- read.csv("/Users/ligeialugli/Dropbox/BTW_Master/BTW_CorpusStuff/BTWCorpusFunctions/NewCorpusMetadata_latest.csv", stringsAsFactors = F)
# MetaVar <- "title"
# collocateBoost <- 5 # added when a collocate is present in sentence, but not per collocate: just presence/absence
# source("./R/SetUp.R")
#
 # GdexRulesDF <- read.csv("/Users/ligeialugli/Dropbox/BTW_Master/BTW_CorpusStuff/BTWCorpusFunctions/FullBuddh_GdexRules.csv",stringsAsFactors = F) #read.csv("/Users/ligeialugli/Dropbox/DemocratizingDigitalLexy_Private/GdexRulesTemplate_test.csv", stringsAsFactors = F)
 # GdexRulesDF[is.na(GdexRulesDF)] <- ""
 # CollocatesDF <- GetDFfromFile(Dir="./data/Outputs",filenameString="CollocateCandidates")
 # HeadFreqs <- fread("./data/Outputs/HeadwordFreqs.csv")
#
# LEM <- 'pratyaya'
# Cores <- 5
# HeadwordVar <- "lemma"
# ShorterPen <- 5
# LongerPen <- 0.5
# LenRange <- 4:12
# wordFormVar <- "word"
# Var <- "NormSuffix"
# SampleSize <- 700
# MinScore <- 0
#
#SentsScoredDF <- FastSorter(CorpusDocsDir="./data/CorpusDocs", HeadwordVar, LEM, GdexRulesDF,CollocatesDF,collocateBoost=2, LenRange,LongerPen,ShorterPen, Cores)
#colnames(SentsScoredDF)
#
# MetaDF <- read.csv("./data/CorpusMetadata.csv", stringsAsFactors = F)
# MetaVar <- "period"
# SAMPLED  <- Sampler(LEM, SentsScoredDF, CollocatesDF, HeadwordVar, wordFormVar, Var, SampleSize, MetaDF, MetaVar, Cores)
# colnames(SAMPLED)
# sampledHoriz <- CreateHorizontalGdexSentDF(SampledSents,wordFormVar)
# sampledHoriz$ID[sampledHoriz$ID > 40]
#View(CreateHorizontalGdexSentDF(SAMPLED,wordFormVar))
# #

#
#
# zu <- data.frame(ID = unique(x$UID), Sent= paste0(x[[wordFormVar]], collapse = " "), Score=unique(x$Score) , x[,colnames(x)[str_detect(colnames(x), "penalty")]])
# colVec <- c(colnames(x)[str_detect(colnames(x), "penalty")])
# x[,..Vac]
#
# filter(x, colnames(x)[str_detect(colnames(x), "penalty")])
# x$genre
