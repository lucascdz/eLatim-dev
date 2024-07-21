JoinAndSum <- function(DF1,DF2, ColToJoinBy){
  gc()


  colnames(DF1) <- gsub("\\.([xy])$",".\\1original",colnames(DF1)) # handles cases where original column names end n .x or .y, to prevent treating them as join by-products
  colnames(DF2) <- gsub("\\.([xy])$",".\\1original",colnames(DF2)) # handles cases where original column names end n .x or .y, to prevent treating them as join by-products
  ColNames <-   colnames(DF1)
  ColNames <- ColNames[ColNames!=ColToJoinBy]

  DF <- full_join(DF1,DF2, by = ColToJoinBy)
  DF[is.na( DF )] <- 0

  for (i in seq_along(ColNames)){
    DF[[ColNames[i]]] <- DF[[paste0(ColNames[i],".x")]] + DF[[paste0(ColNames[i],".y")]]
  }

  DF <-  DF[, !str_detect(colnames(DF),"\\.[xy]")]
  return(DF)
}

JoinAndClean <- function(DF1,DF2, ColToJoinBy){
  gc()

  colnames(DF1) <- gsub("\\.([xy])$",".\\1original",colnames(DF1)) # handles cases where original column names end n .x or .y, to prevent treating them as join by-products
  colnames(DF2) <- gsub("\\.([xy])$",".\\1original",colnames(DF2)) # handles cases where original column names end n .x or .y, to prevent treating them as join by-products
  ColNames <-   colnames(DF1)
  ColNames <- ColNames[ColNames!=ColToJoinBy]

  DF <- full_join(DF1,DF2, by = ColToJoinBy)
  DF[is.na( DF )] <- 0

  DF <-  DF[, !str_detect(colnames(DF),"\\.[xy]")]
  return(DF)
}


GetHeadwordsFreqsDF <- function(HeadwordVar, NormalizeFreqPer=100000, SourceDir, FreqsTablesFiles, Cores){

  if (Sys.info()[['sysname']]!="Windows"){
    AllFreqsTables <- mclapply(FreqsTablesFiles, function(x) read.fst(paste0(SourceDir,"/", x)), mc.cores=Cores)

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
    AllFreqsTables <- parLapply(cl, FreqsTablesFiles, function(x) read.fst(paste0(SourceDir,"/", x)))
    stopCluster(cl)
  }


  names(AllFreqsTables) <- gsub(".fst$","", FreqsTablesFiles)
  AllFreqsTables <- AllFreqsTables[!sapply(AllFreqsTables, is.null)]
  AllFreqsTables <- AllFreqsTables[sapply(AllFreqsTables, nrow) > 0]
  allHeadwordsCorpusFreqs <- Reduce(function(x,y) JoinAndSum(x,y, HeadwordVar), AllFreqsTables)
  gc()
  allHeadwordsCorpusFreqs <- allHeadwordsCorpusFreqs[order(as.character(allHeadwordsCorpusFreqs[[HeadwordVar]])),]
  allHeadwordsCorpusFreqs$NormFreq <- (allHeadwordsCorpusFreqs$Freq*NormalizeFreqPer)/sum(allHeadwordsCorpusFreqs$Freq)

  colnames(allHeadwordsCorpusFreqs)[1] <- "lemma" # to harmonise colname with other outputs and smoother operations

  return(allHeadwordsCorpusFreqs)
}


AddDP_toFreqsDFbyMetaVal <- function(FullFreqsDFbyMetaVal, MetaVal){

  AbsFreqsByMetaValDF <- FullFreqsDFbyMetaVal[, c( which(colnames(FullFreqsDFbyMetaVal)=="lemma" ), which(str_detect(colnames(FullFreqsDFbyMetaVal),"_Freq" )) )]
  AbsFreqsByMetaValDF <- column_to_rownames(AbsFreqsByMetaValDF, "lemma")

  ExpProp <- lapply(colnames(AbsFreqsByMetaValDF), function(x) sum(AbsFreqsByMetaValDF[[x]]))
  Total <- do.call(sum, ExpProp)
  ExpProp <- unlist( ExpProp)/Total
  names(ExpProp) <- colnames(AbsFreqsByMetaValDF)
  ObsProp <- lapply(colnames(AbsFreqsByMetaValDF), function(x) AbsFreqsByMetaValDF[[x]]/rowSums(AbsFreqsByMetaValDF) )
  ObsProp <- do.call(cbind, ObsProp)
  DP <- lapply(1:ncol(ObsProp), function(i) abs(ObsProp[,i]-ExpProp[i][[1]]))
  DP <- do.call(cbind, DP)
  DP  <- rowSums(DP)/2
  AbsFreqsByMetaValDF$DP <- DP
  AbsFreqsByMetaValDF <- rownames_to_column(AbsFreqsByMetaValDF,"lemma")

  FullFreqsDFbyMetaVal[[paste0(MetaVal,"_DP")]] <- AbsFreqsByMetaValDF$DP
  return(FullFreqsDFbyMetaVal)
}


GetFreqsDFbyMetaVal <- function(HeadwordVar, Meta, MetaVal,NormalizeFreqPer, ColToJoinBy, AllTextsSizes,FreqDFsFiles, SourceDir, mcCores){ # MUST REMOVE BIT WITH 2023-05-13Lemmatized


  if(length(unique(Meta[[MetaVal]]))>30){
    stop(paste0("please select a broader Metadata variable '" ,MetaVal , "' is too granular and may result in an excessively big dataset"))
  }
  if(length(unique(Meta[[MetaVal]]))<2){
    stop(print("please select a Metadata variable that takes at least 2 distinct values in your corpus"))
  }

  if(!ColToJoinBy %in% MetaVal){
    MetaSelect <- Meta[, colnames(Meta)[colnames(Meta) %in% c(MetaVal, ColToJoinBy )]]
  }else{

    MetaSelect <- Meta[, colnames(Meta)==ColToJoinBy ]

  }

  AllTextsSizesSelect <- left_join(AllTextsSizes,  MetaSelect, by = ColToJoinBy)






  if (Sys.info()[['sysname']]!="Windows"){
    FreqsDFbyMetaVal <- mclapply(
      unique(Meta[[MetaVal]][ Meta[[ColToJoinBy]] %in% AllTextsSizes[[ColToJoinBy]] ]),
      function(x) GetHeadwordsFreqsDF( HeadwordVar,
                                       NormalizeFreqPer=NormalizeFreqPer,
                                       SourceDir,
                                       na.omit(FreqDFsFiles[
                                         which(FreqDFsFiles %in% paste0("HeadwordFreqs_",
                                                                        AllTextsSizesSelect[[ColToJoinBy]][AllTextsSizesSelect[[MetaVal]]==x],
                                                                        ".fst")
                                         )]), mcCores))

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
    FreqsDFbyMetaVal <- parLapply(cl, unique(Meta[[MetaVal]][ Meta[[ColToJoinBy]] %in% AllTextsSizes[[ColToJoinBy]] ]), function(x) GetHeadwordsFreqsDF( HeadwordVar,
                                                                                                                                                           NormalizeFreqPer=NormalizeFreqPer,
                                                                                                                                                           SourceDir,
                                                                                                                                                           na.omit(FreqDFsFiles[
                                                                                                                                                             which(FreqDFsFiles %in% paste0("HeadwordFreqs_",
                                                                                                                                                                                            AllTextsSizesSelect[[ColToJoinBy]][AllTextsSizesSelect[[MetaVal]]==x],
                                                                                                                                                                                            ".fst")
                                                                                                                                                             )]), Cores))
    stopCluster(cl)
  }




  names(FreqsDFbyMetaVal) <- unique(Meta[[MetaVal]][Meta[[ColToJoinBy]] %in% AllTextsSizes[[ColToJoinBy]]])
  FreqsDFbyMetaVal <- FreqsDFbyMetaVal[!sapply(FreqsDFbyMetaVal, is.null)]
  AllFreqsDFbyMetaVal <- lapply(seq_along(FreqsDFbyMetaVal), function(i)  {colnames(FreqsDFbyMetaVal[i][[1]])[2: length(FreqsDFbyMetaVal[i][[1]])] <- paste(names(FreqsDFbyMetaVal[i]), colnames(FreqsDFbyMetaVal[i][[1]])[2: length(FreqsDFbyMetaVal[i][[1]])], sep="_"); FreqsDFbyMetaVal[i][[1]] })


  FullFreqsDFbyMetaVal <-  Reduce(function(x,y) JoinAndClean(x,y, "lemma"), AllFreqsDFbyMetaVal)

  FullFreqsDFbyMetaVal[is.na(FullFreqsDFbyMetaVal)] <- 0
  return(FullFreqsDFbyMetaVal)
}


GetSubcorporaFreqsAndDP <- function(MetadataPath=NULL, ColToJoinBy, MetaVal,NormalizeFreqPer=100000, HeadwordVar, FreqTablesSourceDir, mcCores){

  FreqDFsFiles <- dir(FreqTablesSourceDir)

  if (Sys.info()[['sysname']]!="Windows"){
    AllTextsSizes <- mclapply(FreqDFsFiles, function(x) data.frame(filename= gsub("^HeadwordFreqs_|\\.fst$", "", x)  , size= sum(read.fst(paste0(FreqTablesSourceDir,"/", x), columns = "Freq")) ))

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
    AllTextsSizes <- parLapply(cl, FreqDFsFiles, function(x) data.frame(filename= gsub("^HeadwordFreqs_|\\.fst$", "", x)  , size= sum(read.fst(paste0(FreqTablesSourceDir,"/", x), columns = "Freq")) ))
    stopCluster(cl)
  }



  AllTextsSizes <- do.call(rbind, AllTextsSizes)
  colnames(AllTextsSizes)[1] <- ColToJoinBy

  if(!is.null(MetadataPath)){

    Meta <- read.csv(MetadataPath,stringsAsFactors = F)
  }else if(file.exists("./data/CorpusData") & length(dir("./data/CorpusData")[str_detect(dir("./data/CorpusData"),"_CorpusDerivedMeta.fst")])>0){

    Meta <- read.fst(paste0("./data/CorpusData/", dir("./data/CorpusData")[str_detect(dir("./data/CorpusData"),"_CorpusDerivedMeta.fst")]))
    Meta$filename <- gsub("\\.[A-z]{2,6}$","",Meta$filename)
    Meta <- as.data.table(Meta)

  }else{
    Meta <- NULL
  }

  if(!is.null(Meta)){

    Meta[is.na(Meta)] <- ""
    if(length(MetaVal)==1){

      FullFreqsDFbyMetaVal <- GetFreqsDFbyMetaVal(HeadwordVar,Meta, MetaVal,NormalizeFreqPer, ColToJoinBy, AllTextsSizes, FreqDFsFiles,FreqTablesSourceDir, mcCores=mcCores)
      FullFreqsDFbyMetaValWithDP <- AddDP_toFreqsDFbyMetaVal(FullFreqsDFbyMetaVal, MetaVal)


    }else if(length(MetaVal)>1){

      FullFreqsDFbyMetaVal_LIST <- lapply(MetaVal, function(x) GetFreqsDFbyMetaVal(HeadwordVar,Meta, x,NormalizeFreqPer, ColToJoinBy, AllTextsSizes,FreqDFsFiles,FreqTablesSourceDir, mcCores=mcCores))
      FullFreqsDFbyMetaValWithDP_LIST <- lapply(seq_along(FullFreqsDFbyMetaVal_LIST), function(i)  AddDP_toFreqsDFbyMetaVal(FullFreqsDFbyMetaVal_LIST[i][[1]], MetaVal[i]))
      FullFreqsDFbyMetaValWithDP <- do.call(cbind, FullFreqsDFbyMetaValWithDP_LIST)

    }
    return(FullFreqsDFbyMetaValWithDP)
  } else{
    print("no metadata file")
  }
}



