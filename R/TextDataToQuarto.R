TextDataToQuarto <- function(FilePath, TargetPath){

  FilePath <- gsub('"',"",FilePath)
  TextDataFileExt <- tools::file_ext(FilePath)


if(TextDataFileExt %in% c("doc","docx","rtf")){
    require(pandoc)
    TextData <- pandoc_convert(file=FilePath,from= TextDataFileExt, to="markdown")
    write(TextData, TargetPath)
  }else if(TextDataFileExt %in% c("txt")){

    require(readtext)
    TextData <- readtext::readtext(FilePath)
    TextData <- TextData$text
    write(TextData, TargetPath)

  }else if(TextDataFileExt %in% c("md","qmd","Rmd")){

    
    if (Sys.info()[['sysname']]!="Windows"){
      
      system(paste("cp",FilePath,TargetPath))
    }else{
      system('powershell.exe', input= paste("cp",FilePath,TargetPath), intern=T)
    }


  }else{
    print("unrecognized file extension, please use txt, md, doc, docx, rtf, Rmd or qmd")
    return(NULL)
  }



}


