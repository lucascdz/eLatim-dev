
# first function to convert tabular dictioanry data to r-list & json

# to do: find tidy way to remove empty cols

# perhaps offer option to aggretate vals from multiple cols into a signle list item
# see e.g. Lucas cols sense_1, sense_2 etc.
AddNumericdisambiguator <- function(DF,NameOfColToDisambiguate){

  if(length(unique(DF[[NameOfColToDisambiguate]])) < nrow(DF) ){

    DF[[NameOfColToDisambiguate]] <-  unlist(lapply(seq_along(1:nrow(DF)), function(i) paste0(DF[[NameOfColToDisambiguate]][i],"_",i) ))


  }

  return(DF)

}


ConvertToRDSandJSON <- function(DictData,
                                DictName="dictData",
                                DataHeadCol,
                                HeadDuplicateHandling="merge", # can be one of 3: c("merge","UseExtraCols","addNumber")
                                HeadDisambiguation = NULL,
                                AdditionalDataPath,
                                ColToJoinBy
                                ){ # for method "UseExtraCols", HeadDisambiguation must be the name of the col to be used for disambiguation


  require(jsonlite)

  if(!is.null(DictName) && !is.na(DictName) && nchar(DictName)<1 ){
    DictName <- "MyDict"
  }else if(!is.null(DictName) && !is.na(DictName) && nchar(DictName)>1 ){
    DictName <- gsub( " ","", DictName)
  }

  if(!is.null(AdditionalDataPath) && AdditionalDataPath!=""){



    FileExt <- gsub("^.*?\\.(zip|csv|tsv|rds)$", "\\1" , AdditionalDataPath)


    if(FileExt %in% c("zip","csv","tsv","rds")){


      AddiData <- ReadAdditionalData(AdditionalDataPath, FileExt)

      if(!is.null(AddiData)){

        if(!is.null(ColToJoinBy) && ColToJoinBy!=""){


          DictData <- JoinDictWithAdditionalData(DictData, AddiData, ColToJoinBy )
        }
      }
    }
  }


  if(!is.null(DataHeadCol)){

    # uncomment the following to get po-up about sparsely populated cols
    NullPerc <- lapply(colnames(DictData), function(x) length(which(DictData[,colnames(DictData)==x]==""))/length(DictData[,colnames(DictData)==x]) )
    ElementToFlag <- colnames(DictData)[which(unlist(NullPerc)>0.4 )]



    if( length(DictData[[DataHeadCol]][duplicated(DictData[[DataHeadCol]])]) >0 ){
      # if DataHead has non unique value,
      # offer option to:
      # aggregate rows with same value , or
      # create unique value from combination with another column (eg lemma+ Pos), or
      # leave them as different entries, in which case they will be followed by a number for disambiguation

      if(HeadDuplicateHandling=="merge"){
        # method merge aggregates rows with same value under a single head
        DictData <- split(DictData, DictData[[DataHeadCol]])
        #DictDataList <- lapply(seq_along(DictData), function(i) DictData[i][[1]][,-which(colnames(DictData[i][[1]])==DataHeadCol)] )
        #names(DictDataList) <- names(DictData)
        #DictData <- DictDataList
      }else if(HeadDuplicateHandling=="UseExtraCols" && HeadDisambiguation!=NULL){
        # need to input which extra col to use for disambiguation, as HeadDisambiguationCol
        DictData$Disambiguation <- paste0(DictData[[DataHeadCol]], "_" ,DictData[[HeadDisambiguation]])
        DictData <- split(DictData, DictData$Disambiguation)

      }else if(HeadDuplicateHandling=="addNumber"){

        DictData <- split(DictData, DictData[[DataHeadCol]])
        NAMES <- names(DictData)
        DictData <- lapply(seq_along(DictData), function(i) AddNumericdisambiguator(DictData[i][[1]], DataHeadCol) )
        names(DictData) <- NAMES

      }else{
        print('unrecognized method to treat duplicated values, please use "merge","UseExtraCols" or "disambiguateWithNumeral" ')
        return(NULL)
      }

      # include additional data if AdditionalDataPath is provided


    }else{
      DictData <- split(DictData, DictData[[DataHeadCol]])
    }

    DictDataJSON <- toJSON(DictData, pretty = T)
    write(DictDataJSON, paste0("./data/" ,DictName,".json"))
    saveRDS(DictData,paste0("./data/" , DictName,".rds"))

    if(interactive() && !is.null(ElementToFlag) && length(ElementToFlag) > 0){

      ColstoCheck <- paste(ElementToFlag, collapse= " ; ")
      Message <- paste("The following column(s) have empty values in more than 40% of cases,
                    consider omitting or restructuring them before converting:", ColstoCheck )

      showModal(modalDialog(
        title = "possible data-design issue",

        div(HTML(Message)),

        easyClose = FALSE
      ))
    }


    return(DictData)
  }
}


