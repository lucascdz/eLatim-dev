
# functions to merge additional tabular files into the main JSON dictionary data
# to be used e.g. to add metadata
# not advisable to merge into main dictionary-data very large datasets, as this may cause memory and performance issues
# large datasets are best inserted in the dictionary when using the interactive digital-dictionary-building app



ReadAdditionalData <- function(DataFilePath, DataFileExt){ # must be path to one tabular file or zip folder containing tabular files in csv, tsv or rds
  
  DataFilePath <- gsub('"','',DataFilePath)
  
  if(DataFileExt=="zip"){
    
    DataFilePath <-  unzip(DataFilePath)
    DataFilePath <- DataFilePath[!str_detect(DataFilePath,"\\._")]
    
    DataFileExt <- unique(tools::file_ext(DataFilePath))
  }
  
  if(length(unique(DataFileExt))==1){
    
    if( unique(DataFileExt)=="csv"){
      require(readr)
      if(length(DataFilePath)<2){
        AdditionalData <- read_csv(DataFilePath) %>% 
          eliminateEmptyCols(.)
        Cols <- colnames(AdditionalData)
      }else{
        AdditionalData <- lapply(DataFilePath, function(x) read_csv(x) %>% eliminateEmptyCols(.))
        Cols <- lapply(AdditionalData, function(x) colnames(x))
        Cols <- Reduce( function(x,y) intersect(x,y), Cols)
        
      }
    }else if(unique(DataFileExt)=="tsv") {
      if(length(DataFilePath)<2){
        AdditionalData <- read.delim(DataFilePath ,stringsAsFactors = F) %>% 
          eliminateEmptyCols(.)
        Cols <- colnames(AdditionalData)
      }else{
        AdditionalData <- lapply(DataFilePath, function(x) read.delim(x, stringsAsFactors = F) %>% eliminateEmptyCols(.))
        Cols <- lapply(AdditionalData, function(x) colnames(x))
        Cols <- Reduce( function(x,y) intersect(x,y), Cols)
      }
      
    }else if(unique(DataFileExt)=="rds") {
      if(length(DataFilePath)<2){
        AdditionalData <- readRDS(DataFilePath) %>% 
          eliminateEmptyCols(.)
        Cols <- colnames(AdditionalData)
      }else{
        AdditionalData <- lapply(DataFilePath, function(x) readRDS(x) %>% eliminateEmptyCols(.))
        Cols <- lapply(AdditionalData, function(x) colnames(x))
        Cols <- Reduce( function(x,y) intersect(x,y), Cols)
      }
      
    }else{
      print("unrecognized file extension. please only use csv, tsv or rds")
      return(NULL)
    }
    
    
    return(list(AdditionalData=AdditionalData , Cols=Cols))
  }else{
    print("heterogenous file extension; all files must have the same extension, which must be one of the following csv, tsv or rds")
    return(NULL)
  }
}

JoinAndClean <- function(DF1,DF2, ColToJoinBy){
  
colnames(DF1) <- gsub("\\.([xy])$",".\\1original",colnames(DF1)) # handles cases where original column names end n .x or .y, to prevent treating them as join by-products
colnames(DF2) <- gsub("\\.([xy])$",".\\1original",colnames(DF2)) # handles cases where original column names end n .x or .y, to prevent treating them as join by-products


 DF <- full_join(DF1,DF2, by = ColToJoinBy)
 colnames(DF) <- gsub("\\.x$","", colnames(DF))
 DF <- DF[, which(!str_detect(colnames(DF), "\\.y$"))]
 colnames(DF) <- gsub("\\.([xy])original$",".\\1", colnames(DF)) 
 return(DF)
}


JoinDictWithAdditionalData <- function(DictData, AdditionalData, ColToJoinBy){
  
  if(!is.null(ColToJoinBy) && ColToJoinBy %in% AdditionalData$Cols){
    
    # print("ColToJoinBy from JoinDictWithAdditionalData()")
    # print(ColToJoinBy)
    # print("ColToJoinBy %in% AdditionalData$Cols from JoinDictWithAdditionalData()")
    # print(ColToJoinBy %in% AdditionalData$Cols)
    
    AdditionalDataTables <- AdditionalData[names(AdditionalData)!="Cols"]
    
    if(!is.data.frame(AdditionalDataTables)){
      AdditionalDataDF <- Reduce(function(x,y) JoinAndClean(x,y, ColToJoinBy), AdditionalDataTables$AdditionalData)
      gc()
    }else{
      AdditionalDataDF <- AdditionalDataTables
    }
    JoinedData <- left_join(DictData, AdditionalDataDF, by = ColToJoinBy)
    colnames(JoinedData) <- gsub("\\.x$","", colnames(JoinedData))
    JoinedData <- JoinedData[, which(!str_detect(colnames(JoinedData), "\\.y$"))]
    
    return(JoinedData)
  }else{
    print("no matching column name in the dictionary data")
  }
}


