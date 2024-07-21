# add  preface and Media to SharableDictData

convertMDToTXT <- function(file, TargetDir){
  if(file.exists(file)){
  convfile <- pandoc_convert(file = file, from = "markdown", to = "plain")
  write(convfile, paste0(TargetDir,"/",unlist(str_split(file,"/")) %>% .[length(.)] ,".txt"))
  }
}

MoveFilesToSharebleDict <- function(Credits, TargetDir="./SharableDictData"){

FilesToMove <- c("./MyDict/index.qmd","./MyDict/references.qmd","./MyDict/Home.Rmd","./MyDict/Preface.Rmd","./MyDict/References.Rmd")


lapply(FilesToMove, function(x) convertMDToTXT(x,TargetDir) )

if(file.exists("./MyDict/Media")){
  if (Sys.info()[['sysname']]!="Windows"){
  system('cp -r ./MyDict/Media ./SharableDictData')
  }else{
  system('powershell.exe', input= 'cp -r ./MyDict/Media ./SharableDictData', intern=T)
  }
}

if(file.exists("./MyDict/AltText")){
  if (Sys.info()[['sysname']]!="Windows"){
    system('cp -r ./MyDict/AltText ./SharableDictData')
  }else{
    system('powershell.exe', input= 'cp -r ./MyDict/AltText ./SharableDictData', intern=T)
  }
}
write(paste(Credits,"This repository contains all the dictionary files. any media files linked in the entries are in the Media subdirectory; alt-text descriptions for data visualizations are available in the AltText directory", sep="\n"), paste0(TargetDir,"/ReadMe.txt"))
}



