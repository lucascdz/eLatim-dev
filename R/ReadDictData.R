
# function to read dictionary data into r pipeline
# readtext section to be checked: for doc/docx probably necessary to first convert to .md/.Rmd

# !! ADD XML reader! !!

eliminateEmptyCols <- function(DF){

  EmptyCols <-  unlist(lapply(colnames(DF), function(x)  if(identical(unique(is.na(DF[[x]])),TRUE)){x}))
  DF <- DF[,which(!colnames(DF) %in% EmptyCols)]
  return(DF)
}


ReadDictData <- function(DictDataFilePath, DictDataFileExt){


  DictDataFilePath <- gsub('"',"",DictDataFilePath)

  print(DictDataFilePath)

  if(DictDataFileExt=="csv"){
    require(readr)
    DictData <- read_csv(DictDataFilePath) %>%
                   eliminateEmptyCols(.)

  }else if(DictDataFileExt=="tsv") {
    DictData <- read.delim(DictDataFilePath ,stringsAsFactors = F) %>%
                                         eliminateEmptyCols(.)

  }else if(DictDataFileExt %in% c("doc","docx","rtf")){
  require(pandoc)
    DictData <- pandoc_convert(file=DictDataFilePath,from= DictDataFileExt, to="html")
    DictData <- paste(DictData, collapse = " ")


  }else if(DictDataFileExt %in% c("txt","md","Rmd", "qmd")){

    require(readtext)

    DictData <- readtext::readtext(DictDataFilePath)
    DictData <- DictData$text

  }else if(DictDataFileExt=="rds"){
    DictData <- readRDS(DictDataFilePath)
  }else{
    print("unrecognized file extension, please use csv, tsv, rds, txt, md, doc, docx, rtf, Rmd or qmd")
    return(NULL)
  }

  return(DictData)
}


ReadDictDataWithZip <- function(DictDataFilePath, DictDataFileExt){
  print("from ReadDictDataWithZip:")
  print(DictDataFileExt)

  DictDataFilePath <- gsub('"',"",DictDataFilePath)

  if(DictDataFileExt=="zip"){

    DictDataFilePath <-  unzip(DictDataFilePath)
    DictDataFilePath <- DictDataFilePath[!str_detect(DictDataFilePath,"\\._")]

    DictDataFileExt <- unique(tools::file_ext(DictDataFilePath))
  }


  if(DictDataFileExt=="csv"){
    require(readr)
    if(length(DictDataFilePath)<2){
      DictData <- read_csv(DictDataFilePath)
      print(head(DictData))
    }else{
      DictData <- lapply(DictDataFilePath, function(x) read_csv(x) %>%
                           eliminateEmptyCols(.))
      DictData <- do.call(rbind, DictData)
    }
  }else if(DictDataFileExt=="tsv") {
    if(length(DictDataFilePath)<2){
      DictData <- read.delim(DictDataFilePath ,stringsAsFactors = F) %>%
        eliminateEmptyCols(.)

    }else{
      DictData <- lapply(DictDataFilePath, function(x) read.delim(x, stringsAsFactors = F) %>%
                           eliminateEmptyCols(.))
      DictData <- do.call(rbind, DictData)
    }

  }else if(DictDataFileExt=="rds") {
    if(length(DictDataFilePath)<2){
      DictData <- readRDS(DictDataFilePath)

    }else{
      DictData <- lapply(DictDataFilePath, function(x) readRDS(x))
      DictData <- do.call(rbind, DictData)
    }
  }else if(DictDataFileExt %in% c("doc","docx","rtf", "txt", "md")){
    require(pandoc)
    if(DictDataFileExt %in% c("txt","md")){
      DictDataFileExt <- "markdown"
    }
    if(length(DictDataFilePath)<2){
      DictData <- pandoc_convert(file=DictDataFilePath,from= DictDataFileExt, to="html")
      DictData <- paste(DictData, collapse = " ")
    }else{
      DictData <- lapply(DictDataFilePath, function(x) {pandoc_convert(file=x,from= DictDataFileExt, to="html") %>%
          paste(., collapse = " ")})
    }

  }else if(DictDataFileExt=="pdf"){
    print("from pdf section")
      if(length(DictDataFilePath)<2){
        DictData <- pdftools::pdf_text(DictDataFilePath)
        print(DictData)
      }else{
        DictData <- lapply(DictDataFilePath, function(x) pdftools::pdf_text(x))
        DictData <- do.call(rbind, DictData)
      }
    }else{
      print("unrecognized file extension, please use csv, tsv, rds, txt, md, doc, docx, rtf, pdf, Rmd or qmd")
      return(NULL)
    }

  return(DictData)
}





