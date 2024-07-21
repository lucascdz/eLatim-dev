
GetAvailableData <- function(DictData){
  if(!is.null(DictData)){
    availableVars <- "DictData" #names(wordData)
  }else{
    availableVars <- ""
  }

  if(file.exists("./data/Plots")){
    AvailablePlots <- unique(unlist(str_extract(dir("./data/Plots"), "^(.*?)_") ))
    AvailablePlots <- gsub("_$","",AvailablePlots)
    AvailablePlots <- paste0("Plots_" ,AvailablePlots)
  }else{
    AvailablePlots <-""
  }

  if(file.exists("./data/Media")){
    AvailableMedia <- unique(unlist(str_extract(dir("./data/Media"), "^(.*?)_") ))
    AvailableMedia <- gsub("_$","",AvailableMedia)
    AvailableMedia <- paste0("Media_" ,AvailableMedia)

  }else{
    AvailableMedia <-""
  }

  if(file.exists("./data/LexicalData")){
    AvailableLexicalData <- dir("./data/LexicalData")
    AvailableLexicalData <- paste0("LexicalData_" ,AvailableLexicalData)
  }else{
    AvailableLexicalData <-""
  }

  if(file.exists("./data/Outputs")){
    AvailableOutputs <- dir("./data/Outputs")
    AvailableOutputs <- paste0("Outputs_" ,AvailableOutputs)
  }else{
    AvailableOutputs <-""
  }

  AvailableData <- c(availableVars, AvailablePlots,AvailableOutputs, AvailableMedia, AvailableLexicalData)
  AvailableData <- AvailableData[AvailableData!=""]
  return(AvailableData)
}

GetAvailableVars <- function(DataSource, DictData, Word){

  print(paste("Word fromGetAvailableVars:", Word))

  if (DataSource == "DictData"){
    AvailableVars <- c("full dataset", names(DictData[[Word]]))
  }else if(str_detect(DataSource, "^(Outputs|LexicalData)_")){
    filepath <- gsub("^(Outputs|LexicalData)_(.*?)$","./data/\\1/\\2",DataSource)
    AvailableVars <- c("full dataset", names(read.csv(filepath)))
  }else if(str_detect(DataSource,"^Plots")){
    DIRfiles <- dir("./data/Plots")
    contentType <- gsub("^Plots_","", DataSource)
    AvailableVars <- DIRfiles[str_detect(DIRfiles, paste0("^", contentType,".*?", MakeSafeForFilename(Word)))]


  }else{
    DIRfiles <- dir("./data/Media")
    contentType <- gsub("^Media_","",DataSource)
    AvailableVars <- DIRfiles[str_detect(DIRfiles, paste0("^", contentType,".*?", Word))]
  }

  return(unique(AvailableVars))

}

GetDataFromFile <- function(DataSource, DictData, Var, Word){

  if (DataSource == "DictData"){
    if(!identical(Var, "full dataset")){
      Data <- DictData[[Word]][names(DictData[[Word]]) %in% Var]
    }else{
      Data <- DictData[[Word]]
    }
  }else if(str_detect(DataSource, "^(Outputs|LexicalData)_")){
    filepath <- gsub("^(Outputs|LexicalData)_(.*?)$","./data/\\1/\\2",DataSource)
    Data <- read.csv(filepath, stringsAsFactors = F)
    Data <- Data[Data$lemma==Word,]
    if(length(Data)>2){
      Data <- Data[,-(which(colnames(Data)=="lemma"))] # remove lemma col
    }
    if(!identical(Var, "full dataset")){
      Data <- Data[, colnames(Data) %in% Var]
    }
  }else if(str_detect(DataSource,"^Plots")){
    # move plot to MyDict/Media folder for use in Quarto project
    if(!file.exists("./MyDict/Media")){

      if (Sys.info()[['sysname']]!="Windows"){

        system("mkdir ./MyDict/Media")
      }else{
        shell("mkdir .\\MyDict\\Media")
      }
    }
    Var <- str_replace(Var, "^(.*?)_xx[\\d-]+xx_.*?(_.*?)$", paste0("\\1", MakeSafeForFilename(Word),"\\2"))
    if(length(Var)==1 && file.exists(paste0("./data/Plots/",Var))){
      if(str_detect(Var,"png$")){
        Data <- paste0("./MyDict/Media/" , gsub("png$","webp", Var))
        # convert to webp & move
        system(paste0('cwebp -q 80 ./data/Plots/',Var, ' -o ', Data))
      }else if(length(Var)==1 && str_detect(Var,"html$")){
        Data <- paste0("./MyDict/Media/" , Var)
        system(paste0('cp ./data/Plots/',Var, " ", Data))
      }
    }
  }else{
    # move media file to MyDict/Media folder for use in Quarto project
    if(!file.exists("./MyDict/Media")){

      if (Sys.info()[['sysname']]!="Windows"){

        system("mkdir ./MyDict/Media")
      }else{
        shell("mkdir .\\MyDict\\Media")
      }
    }
    Var <- str_replace(Var, "^(.*?)_xx[\\d-]+xx_.*?(_.*?)$", paste0("\\1", MakeSafeForFilename(Word),"\\2"))
    if(length(Var)==1 &&  file.exists(paste0("./Media/",Var))){
      Data <- paste0("./MyDict/Media/" , Var)
      if (Sys.info()[['sysname']]!="Windows"){
        system(paste0('cp ./Media/',Var, " ", Data))
      }else{
        system('powershell.exe', input= paste0('cp ./Media/',Var, " ", Data), intern=T)
      }
    }
  }
  if(exists("Data") && !is.null(Data) && length(Data)>0){
    return(unique(Data))
  }
}


MakeDataContent <- function(DictData, DataSource, Var, Word,Text=""){
  if(!is.null(Var) && length(Var)>0){
    Data <- GetDataFromFile(DataSource, DictData, Var, Word)
    Data[is.na(Data)] <- ""
    if(exists("Data") && !is.null(Data) && length(Data)>0){
      if(!is.null(Text) && Text!=""){
        if(length(Var)==1 && str_detect(Var, "png$")){
          Var <- Data
        }
        if(is.data.frame(Data) && nrow(Data)<1){ # avoid errors in case of lemmas not included in DataSource
          Data <- Data
        }else{
          Data <- MakeTextFromData(Text, Var, Data, DataSource, DictData, Word)
        }
      }
      return(Data)
    }
  }
}

CreateTextContent <- function(TextBlock, Var, FilterVar=NULL,Data, DataSource, DictData, Word){
  TextBlock <- gsub('"','\\\\"',TextBlock)
  TextBlock <- gsub("'","\\\\'",TextBlock)

  if(!is.null(FilterVar)){
    Var <- setdiff(Var, FilterVar)
  }

  if(str_detect(TextBlock, "^%paste ")){

    if(is.atomic(Data)){
      return(Data) # this covers cases where where data is a single value
    }else{

    TextBlock <- gsub("%paste\\s+","%paste ",TextBlock)
    AllWords <- unlist(str_split(TextBlock," "))
    AllWords[!AllWords %in% Var] <- paste0("'", AllWords[!AllWords %in% Var], "'")

    if(!is.null(names(Data))){
      AllWords[AllWords %in% Var] <- paste0("Data", "$", AllWords[AllWords %in% Var])
    }else if(!is.null(names(DataSource))){
      AllWords[AllWords %in% Var] <- paste0(DataSource, "$`", Word, "`$", AllWords[AllWords %in% Var])
    }else{
      # bypass eval as Data's names are null and eval would throw error as there is no object with name==Var
      return(paste(unlist(Data), collapse="; "))
    }
    command <- paste(AllWords[2:length(AllWords)], collapse = ", ")
    command <- paste("paste(", command, ", collapse = '; ')")
    ResultString <- eval( parse(text=command))
  }
  }else if(str_detect(TextBlock, "^%width ") & length(Var)==1 && str_detect(Var,"\\.webp$")){
    Var <- Data # to preserve change of Word in filename
    TextBlock <- gsub("%width\\s+","%width ",TextBlock)
    ResultString <- paste0(Data," {width=",gsub("^%width (.*?)$","\\1", TextBlock),"}")
  }else{
    ResultString <-  TextBlock
  }

  if(!is.null(ResultString)){
    if(length(ResultString)>1){
    ResultString <- ResultString[ResultString!=""]
    }
    if(is.null(FilterVar)){
      ResultString <- paste(ResultString, collapse = " ")
    }else{
      ResultString <- data.frame(ResultString, Data[,colnames(Data) %in% FilterVar])
    }
    if(!is.null(ResultString) && !is.na(ResultString) && sum(nchar(ResultString)) >0){
      return(ResultString)
     }
    }
  }


MakeTextFromData <- function(Text, Var, Data, DataSource, DictData, Word){
  if(!is.null(Text) && str_detect(Text, "%%%(splitby|paste|width|filterby)")){
    TextBlocks <- unlist(str_split(Text, "%%"))
    TextBlocks <- TextBlocks[TextBlocks!=""]

    if(str_detect(Text, "%%%filterby")){
      FilterVar <- unlist(str_split(str_remove(TextBlocks[str_detect(TextBlocks, "%filterby")],"%filterby ")," "))
      FilterVar <- FilterVar[FilterVar!=""]
    }else{
      FilterVar <- NULL
    }

    TextBlocks <- TextBlocks[!str_detect(TextBlocks,"%filterby")]
   if(length(TextBlocks)>0){
    if(str_detect(Text, "%%%splitby")){
      by <- unlist(str_split(TextBlocks[str_detect(TextBlocks, "%splitby")], " "))[2]
      Data <- split(Data, Data[[by]])
      TextBlocks <- TextBlocks[!str_detect(TextBlocks, "%splitby")]
      Content <- lapply(Data, function(D) lapply(TextBlocks, function(x) CreateTextContent(x, Var, FilterVar, D)))
      Content <- lapply(seq_along(Content), function(i) {Cont <- paste(unlist(Content[i][[1]]), collapse = "\n\n"); paste0("#### ", names(Content[i]),"\\n",Cont )}  )
      Content <- paste(Content, collapse=" \n\n")
    }else{
      if(length(TextBlocks)>1){
        Content <- lapply(TextBlocks, function(x) CreateTextContent(x, Var, FilterVar, Data, DataSource, DictData, Word))
        if(is.null(FilterVar)){
          Content <- paste( sapply(Content, paste, collapse=" \n\n "),collapse=" \n\n ")
        }
      }else{
        Content <-  CreateTextContent(TextBlocks, Var, FilterVar, Data,DataSource, DictData, Word)
        if(length(unlist(Content))>1){
          Content <- paste( sapply(Content, paste, collapse=" \n "),collapse=" \n ")

        }
      }
    }
    return(Content)
   }else{
    return(Data)
  }
}
}

OldCreateTextContent <- function(TextBlock, Var, Data, DataSource, DictData, Word){
  TextBlock <- gsub('"','\\\\"',TextBlock)
  TextBlock <- gsub("'","\\\\'",TextBlock)

  if(str_detect(TextBlock, "^%paste ")){
    TextBlock <- gsub("%paste\\s+","%paste ",TextBlock)
    AllWords <- unlist(str_split(TextBlock," "))
    AllWords[!AllWords %in% Var] <- paste0("'", AllWords[!AllWords %in% Var], "'")
    if(!is.null(names(Data))){
    AllWords[AllWords %in% Var] <- paste0("Data", "$", AllWords[AllWords %in% Var])
    }else{
      AllWords[AllWords %in% Var] <- paste0(DataSource, "$`", Word, "`$", AllWords[AllWords %in% Var])
    }
    command <- paste(AllWords[2:length(AllWords)], collapse = ", ")
    command <- paste("paste(", command, ")")
    ResultString <- eval( parse(text=command))

  }else if(str_detect(TextBlock, "^%width ") & length(Var)==1 && str_detect(Var,"\\.webp$")){
    Var <- Data # to preserve change of Word in filename
    TextBlock <- gsub("%width\\s+","%width ",TextBlock)
    ResultString <- paste0(Data," {width=",gsub("^%width (.*?)$","\\1", TextBlock),"}")
  }else{
    ResultString <-  TextBlock
  }
  if(!is.null(ResultString) && length(ResultString)>1){
    ResultString <- ResultString[ResultString!=""]
    ResultString <- paste(ResultString, collapse = " ")
  }
  if(!is.null(ResultString) && !is.na(ResultString) && sum(nchar(ResultString)) >0){
    return(ResultString)
  }
}


OldMakeTextFromData <- function(Text, Var, Data, DataSource, DictData, Word){
  if(!is.null(Text) && str_detect(Text, "%%%(splitby|paste|width)")){
    TextBlocks <- unlist(str_split(Text, "%%"))
    TextBlocks <- TextBlocks[TextBlocks!=""]
    if(str_detect(Text, "%%%splitby")){
      by <- unlist(str_split(TextBlocks[str_detect(TextBlocks, "%splitby")], " "))[2]
      Data <- split(Data, Data[[by]])
      TextBlocks <- TextBlocks[!str_detect(TextBlocks, "%splitby")]
      Content <- lapply(Data, function(D) lapply(TextBlocks, function(x) CreateTextContent(x, Var, D)))
      Content <- lapply(seq_along(Content), function(i) {Cont <- paste(unlist(Content[i][[1]]), collapse = "\n\n"); paste0("#### ", names(Content[i]),"\\n",Cont )}  )
      Content <- paste(Content, collapse=" \n\n")
    }else{
      if(length(TextBlocks)>1){
        Content <- lapply(TextBlocks, function(x) CreateTextContent(x, Var, Data, DataSource, DictData, Word))
        Content <- paste( sapply(Content, paste, collapse=" \n\n "),collapse=" \n\n ")
      }else{
        Content <-  CreateTextContent(TextBlocks, Var, Data,DataSource, DictData, Word)
      }
    }
    return(Content)
  }
}

AssembleEntry <- function(DictData, RuleList, Word, projectType){
if(nchar(paste0("Entry", MakeSafeForFilename(Word),".rds")) < 257){ # longer strings are not permitted as filenames
  AllNewDictData <- lapply(seq_along(RuleList), function(i) MakeDataContent(DictData, RuleList[i][[1]]$Dataset, RuleList[i][[1]]$Vars ,Word, RuleList[i][[1]]$Text ) )
  names(AllNewDictData) <- names(RuleList)
  AllNewDictData <- list(AllNewDictData)
  names(AllNewDictData) <- Word
  if(projectType=="book"){
  saveRDS(AllNewDictData, paste0("./MyDict/Entry", MakeSafeForFilename(Word),".rds"))
  }else{
    saveRDS(AllNewDictData, paste0("./MyDict/data/Entry_", SafeEncodeUTF8(Word),".rds"))
  }
}else{
   print(paste("headword", Word, "exceeds permitted character length"))
 }
}

# lapply(c("kalpa","vikalpa","saṃjñā","prajñapti"), function(x) AssembleEntry(DictData, RuleList,x))
#
# DictData <- read.csv("./data/dictData.csv",stringsAsFactors = F)

# RuleList[6][[1]]$Vars <- c("ID","Sent","genre","period")
# RuleList[6][[1]]$Text <- "%%%filterby genre period %%%paste Sent [ ID ] "
# saveRDS(RuleList,"./DictMakingRuleList.rds")

# AssembleEntry(readRDS("./data/dictData.rds"), readRDS("./DictMakingRuleList.rds"),Word)
   # DictData <- readRDS("./data/dictData.rds")
   # RuleList <- readRDS("./DictMakingRuleList.rds")
   #  Word <- "སྤྲོད་"#names(DictData)[1]
# #
# DictData <- readRDS("./data/dictData.rds")
# RuleList <- readRDS("./DictMakingRuleList.rds")
# Word <- "artha"
#
#  i <- 9
# DataSource <- RuleList[i][[1]]$Dataset
# Var <- RuleList[i][[1]]$Vars
# Text <- RuleList[i][[1]]$Text
# i <- 3
#  Data <- GetDataFromFile(DataSource, DictData, Var, Word)
# Word <- "śabda"
  # DataSource <- RuleList[1][[1]]$Dataset
  # Var <- RuleList[1][[1]]$Vars
  # Text <- RuleList[1][[1]]$Text
#GetDataFromFile(DataSource, DictData, RuleList[1][[1]]$Vars, Word)

#dir("./MyDict/Media")



