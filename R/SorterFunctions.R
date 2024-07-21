
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
GetSimilarsents <- function(SampledSents, simThreshold=0.8, HeadwordVar , nameSent1){

  SimSents <-  lapply(names(SampledSents),
                      function(y) if(nameSent1!=y & (length(SampledSents[[nameSent1]][[HeadwordVar]][SampledSents[[nameSent1]][[HeadwordVar]] %in% SampledSents[[y]][[HeadwordVar]] ])/length(SampledSents[[nameSent1]][[HeadwordVar]])) > simThreshold){
                        data.frame(Sent1=nameSent1,Sent2=y,Sim=length(SampledSents[[nameSent1]][[HeadwordVar]][SampledSents[[nameSent1]][[HeadwordVar]] %in% SampledSents[[y]][[HeadwordVar]]])/length(SampledSents[[nameSent1]][[HeadwordVar]]))})

  SimSentsDF <- do.call(rbind, SimSents)
  return(SimSentsDF)
}
RemoveSimilarSents <- function(SentsDF,simThreshold=0.75,HeadwordVar,Cores ){
  # returns DF with non-similar lemmatized Sents
  # SentsScoredDF must be a DF with lemmatized, vertical sentences and a UID column to identify each sentence
  SentsSample <- split(SentsDF, SentsDF$UID)
  SimilarSents <- lapply(names(SentsSample),
                         function(x) GetSimilarsents(SentsSample, simThreshold, HeadwordVar , x))
  SimilarSents <- do.call(rbind, SimilarSents)
  return(SentsDF[!SentsDF$UID %in% SimilarSents$Sent2,])
}
SamplerPerVarCollo <- function(LEM, SentsScoredDF, CollocatesDF, HeadwordVar, Var, SampleSize, Cores){
  # sample top scoring Sent With each Collocate
  SampledUIDs <- ""
  if(is.null(CollocatesDF) & is.null(Var)){
    SampledUIDs <- unique(SentsScoredDF$UID[order(SentsScoredDF$Score, decreasing = T)])[1:SampleSize]
  }else{
  if(!is.null(CollocatesDF)){
    Collos <- CollocatesDF$collocate[CollocatesDF$lemma==LEM]
    if(!is.null(Collos) && length(Collos) > 0){
      SampledUIDs <- unique(unlist(lapply(Collos, function(x) SampleSentsByVal(SentsScoredDF, x,Var = HeadwordVar, samplesize=3))))
    }
  }
  if(!is.null(Var)){
    vars <- unique(SentsScoredDF[[Var]][SentsScoredDF[[HeadwordVar]]==LEM])
    if(!is.null(vars) && length(vars) > 0){
      SampledUIDs <- c(SampledUIDs, unique(unlist(lapply(vars, function(x) SampleSentsByVal(SentsScoredDF[!SentsScoredDF$UID %in% SampledUIDs,], x,Var = Var, samplesize=3)))))
    }
   }
 }



  # remove similar and adjust to Sample Size
  SampledSents <- RemoveSimilarSents(SentsScoredDF[SentsScoredDF$UID %in% SampledUIDs,], simThreshold=0.8, HeadwordVar,Cores )

  # adjust to SampleSize:
  SentScores <- unique(SampledSents[,colnames(SampledSents) %in% c("UID","Score")])
  SentScores <- SentScores[order(SentScores$Score, decreasing = T),]

  if(length(unique(SampledSents$UID)) < SampleSize & length(unique(SampledSents$UID)) < length(unique(SentsScoredDF$UID[!SentsScoredDF$UID %in% SentScores$UID] ))){
    NewSents <- SentsScoredDF[!SentsScoredDF$UID %in% SentScores$UID,]
    NewSentsScores <- unique(NewSents[,colnames(NewSents) %in% c("UID","Score")])
    NewSentsScores <- NewSentsScores[order(NewSentsScores$Score, decreasing = T),]
    if(nrow(NewSentsScores)>=2*SampleSize){
      NtoSample <- 2*SampleSize
    }else{
      NtoSample <- nrow(NewSentsScores)
    }
    NewSents <- NewSents[NewSents$UID %in% NewSentsScores$UID[1:NtoSample],]
    SampledSents <- rbind(SampledSents, NewSents)
    SampledSents <- RemoveSimilarSents(SentsScoredDF[SentsScoredDF$UID %in% SampledUIDs,], simThreshold=0.8, HeadwordVar,Cores )
  }

  if(length(unique(SampledSents$UID)) > SampleSize){
    if(nrow(SentScores[SentScores$Score==max(SentScores$Score),]) >= SampleSize){
      # sample from max scoring sents
      SampledSents <- SampledSents[SampledSents$UID %in% sample(SentScores$UID[SentScores$Score==max(SentScores$Score)], SampleSize),]
    }else{
      # take top-scoring sents
      SampledSents <- SampledSents[SampledSents$UID %in% unique(SentScores$UID)[1:SampleSize],] #  SentScores$UID[1:SampleSize]
    }
  }
  return(SampledSents)
}
Sampler <- function(LEM, SentsScoredDF, CollocatesDF, HeadwordVar, wordFormVar, Var, SampleSize, MetaDF=NULL, MetaVar=NULL, MinScore, Cores){

  # if MetaVar is used, a numebr of sents = SampleSize will be sampled PER EACH MetaVar.
  if(!is.null(MetaDF) && !is.null(MetaVar)){
    # create metavar Col in SentsScoredDF
    MetaVals <- unique(MetaDF[[MetaVar]])
    SentsScoredDF$filename <- gsub("^\\d+___(.*?)$","\\1" ,SentsScoredDF$UID)
    SentsScoredByMeta <- left_join(SentsScoredDF, MetaDF[, colnames(MetaDF) %in% c("filename", MetaVar)], by="filename" )
    SentsScoredByMeta <- split(SentsScoredByMeta, SentsScoredByMeta[[MetaVar]])
    sampledsentsDF <- lapply(SentsScoredByMeta, function(x) SamplerPerVarCollo(LEM, x, CollocatesDF, HeadwordVar, Var, SampleSize, Cores))
    sampledsentsDF <- do.call(rbind, sampledsentsDF)
  }else{
    sampledsentsDF <- SamplerPerVarCollo(LEM, SentsScoredDF, CollocatesDF, HeadwordVar, Var, SampleSize, Cores)
  }

  # add bold tag around lemma

  sampledsentsDF[[wordFormVar]][sampledsentsDF[[HeadwordVar]]==LEM] <- paste0("<b>",sampledsentsDF[[wordFormVar]][sampledsentsDF[[HeadwordVar]]==LEM] ,"</b>")

  if(!is.null(MinScore) && is.numeric(MinScore)){
    sampledsentsDF <- sampledsentsDF[sampledsentsDF$Score > MinScore,]
  }

  if(length(unique(sampledsentsDF$UID)) > SampleSize){
    if(length(sampledsentsDF$UID[sampledsentsDF$Score==max(sampledsentsDF$Score)]) >= SampleSize){
      # sample from max scoring sents
      sampledsentsDF <- sampledsentsDF[sampledsentsDF$UID %in% sample(sampledsentsDF$UID[sampledsentsDF$Score==max(sampledsentsDF$Score)], SampleSize),]
    }else{
      # take top-scoring sents
      sampledsentsDF <- sampledsentsDF[sampledsentsDF$UID %in% unique(sampledsentsDF$UID)[1:SampleSize],]
    }
    if(!is.null(MinScore) && is.numeric(MinScore)){
      sampledsentsDF <- sampledsentsDF[sampledsentsDF$Score > MinScore,]
    }
  }

  return(sampledsentsDF)
}

SortAndSample <- function(CorpusDocsDir="./data/CorpusDocs", HeadFreqs, HeadwordVar, wordFormVar, LEM, GdexRulesDF,CollocatesDF,collocateBoost, LenRange, LongerPen, ShorterPen, Var, SampleSize, MetaDF=NULL, MetaVar=NULL, MinScore=NULL, AdditionalVars="", Cores){

  if(nrow(HeadFreqs[HeadFreqs$lemma==LEM,])==1){
    SentsScoredDF  <- FastSorter(CorpusDocsDir, HeadwordVar, LEM, GdexRulesDF,CollocatesDF,collocateBoost, LenRange,LongerPen,ShorterPen, Cores)

    sampledSentsDF <- Sampler(LEM, SentsScoredDF, CollocatesDF, HeadwordVar, wordFormVar, Var, SampleSize, MetaDF, MetaVar, MinScore, Cores)

    if(!is.null(MinScore)){
      sampledSentsDF <- sampledSentsDF[sampledSentsDF$Score >= MinScore,]
    }
if(length(AdditionalVars)==0){
    sampledSentsDF <- sampledSentsDF[,colnames(sampledSentsDF) %in% c(wordFormVar, "UID","Score",colnames(sampledSentsDF)[str_detect(colnames(sampledSentsDF),"penalty")])]
}else{
    sampledSentsDF <- sampledSentsDF[,colnames(sampledSentsDF) %in% c(wordFormVar, "UID","Score",colnames(sampledSentsDF)[str_detect(colnames(sampledSentsDF),"penalty")],AdditionalVars)]
}

    return(sampledSentsDF[order(sampledSentsDF$Score, decreasing = T),])

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
