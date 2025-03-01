# logic of data gathering for digital dict
# EntryData <- readRDS(paste0("./MyDict/",dir("./MyDict")[str_detect(dir("./MyDict"), "^Entry_")][1]))
#EntryData <- readRDS(paste0("./MyDict/",dir("./MyDict")[str_detect(dir("./MyDict"), "^Entry_xx114-97-107-7779-97-7751-97xx_.*?.rds")]))

#EntryData[1][[1]][5]
# dataDF<-EntryData[1][[1]][2]
#dataDF <- EntryData[1][[1]][5]
GetTABS <- function(InstructionsFilePath){
  if(file.exists(InstructionsFilePath)){
    Instructions <- readtext(InstructionsFilePath)$text
    if(str_detect(Instructions, "TAB\\(.*?\\)")){
      TABS <- unlist(str_extract_all(Instructions, "TAB\\(.*?\\)"))
      TABS <- lapply(TABS, function(x) unlist(str_split(gsub("TAB|\\(|\\)","",x),";")))
    }else{
      TABS <- NULL
    }
  }else{
    TABS <- NULL
  }
  return(TABS)
}

createEntryContent <- function(dataDF=EntryData[1][[1]][i], dataDFString=paste0('EntryData[1][[1]][',i,'][[1]]'), TABS){

  if(is.null(dataDF[1][[1]][1][[1]])||length(dataDF[1][[1]][1][[1]])==0){
    dataDF[[1]] <- "no data"
  }
  if(is.data.frame(dataDF[[1]])){
    if(nrow(dataDF[[1]])>0 && nchar(paste(dataDF[[1]], collapse = ""))>0){
    content <- paste0("\n#### ", names(dataDF),
                      "\n\n```{r echo=FALSE}\n\n",
                      "rmarkdown::paged_table(",
                      dataDFString,')\n\n```')
    }else{
      content <- paste0("\n#### ", names(dataDF),
                        "\n\n no data \n\n")
    }

  }else if(TRUE %in% str_detect(dataDF[[1]], "\\.webp")){
    if(TRUE %in% str_detect(dataDF[[1]], "\\}$")){
      Bits <- unlist(str_split(dataDF[[1]], " "))
      content <-   paste0(
        "\n#### ", names(dataDF),"\n\n",
        "![](", gsub("./MyDict/", "./", Bits[1]), "){width=50%}",Bits[2] ,"\n\n")
    }else{
      content <-   paste0(
        "\n#### ", names(dataDF),"\n\n",
        "![](", gsub("./MyDict/", "./", dataDF[[1]]) ,"){width=50% fig-alt='",
        readtext(str_replace(dataDF[[1]], "./MyDict/Media/","./data/AltText/") %>%
                   str_replace(.,".webp","_AltText.txt"))$text , "'}\n\n")
    }
  }else if(TRUE %in% str_detect(dataDF[[1]], "html$")){

    content <- paste0("\n#### ", names(dataDF),
                      "\n\n",
                      "<iframe width='900' height='700' src='",
                      gsub("./MyDict/","", dataDF[[1]]),
                      "'>\n\n</iframe>\n\n")

  }else{


    if(!is.null(names(dataDF[[1]]))){
      content <- paste0("\n#### ", names(dataDF), "\n\n" ,paste(dataDF[[1]][[names(dataDF[[1]])]], collapse = "\n\n"),"\n\n")

    }else{
      content <- paste0("\n#### ", names(dataDF), "\n\n" ,dataDF[[1]],"\n\n")
    }
  }

  if(!is.null(TABS) && names(dataDF) %in% unlist(TABS)){
  TAB <- unlist(Filter(function(x) names(dataDF) %in% x, TABS))
        if(names(dataDF)==TAB[1]){
          content <- paste0('\n::: panel-tabset\n',content)
        }else if(names(dataDF)==TAB[length(TAB)]){
          content <- paste0(content, '\n:::\n')
        }
  }
    return(content)
}

createEntryContentNoTAB <- function(dataDF=EntryData[1][[1]][i], dataDFString=paste0('EntryData[1][[1]][',i,'][[1]]')){

  if(is.null(dataDF[1][[1]][1][[1]])||length(dataDF[1][[1]][1][[1]])==0){
    dataDF[[1]] <- "no data"
  }
  if(is.data.frame(dataDF[[1]])){
    if(nrow(dataDF[[1]])>0 && nchar(paste(dataDF[[1]], collapse = ""))>0){
      content <- paste0("\n#### ", names(dataDF),
                        "\n\n```{r echo=FALSE}\n\n",
                        "rmarkdown::paged_table(",
                        dataDFString,')\n\n```')
    }else{
      content <- paste0("\n#### ", names(dataDF),
                        "\n\n no data \n\n")
    }

  }else if(str_detect(dataDF[[1]], "\\.webp")){
      if(str_detect(dataDF[[1]], "\\}$")){
        Bits <- unlist(str_split(dataDF[[1]], " "))
        content <-   paste0(
          "\n#### ", names(dataDF),"\n\n",
          "![](", gsub("./MyDict/", "./", Bits[1]), ")",Bits[2] ,"\n\n")
      }else{
        content <-   paste0(
          "\n#### ", names(dataDF),"\n\n",
          "![](", gsub("./MyDict/", "./", dataDF[[1]]) ,")\n\n")
      }
  }else if(str_detect(dataDF[[1]], "html$")){

    content <- paste0("\n#### ", names(dataDF),
                      "\n\n",
                      "<iframe width='700' height='700' src='",
                      gsub("./MyDict/","", dataDF[[1]]),
                     "'>\n\n</iframe>\n\n")

  }else{


    if(!is.null(names(dataDF[[1]]))){
      content <- paste0("\n#### ", names(dataDF), "\n\n" ,paste(dataDF[[1]][[names(dataDF[[1]])]], collapse = "\n\n"),"\n\n")

    }else{
    content <- paste0("\n#### ", names(dataDF), "\n\n" ,dataDF[[1]],"\n\n")
    }
  }

}

MakeQuartoFile <- function(Word, EntryData,TAB=NULL){


Header <-
'---
title: "WORD"
format: html
editor: visual
toc: false
---
'
Header <- gsub("WORD",Word,Header)
SetUp <-paste0('```{r  setUp, include=FALSE}\n library(tidyverse) \n
      EntryData <- readRDS(paste0("./", dir("./")[str_detect(dir("./"), "', gsub(Word,".*?" ,MakeSafeForFilename(Word)),'.rds")]))',
              '\n\n```')

Contents <- lapply(seq_along(EntryData[1][[1]]), function(i) createEntryContent(dataDF=EntryData[1][[1]][i],  dataDFString=paste0('EntryData[1][[1]][',i,'][[1]]'), TAB) )
Content <- paste(Contents, collapse="\n\n")

EntryQMD <- paste(Header,SetUp,Content, sep="\n\n")

WordforFilename <- str_replace_all(Word, "[^[[:lower:]][[:upper:]]\\d\\s-{}]"," ")
WordforFilename <-gsub("^\\s+|\\s+$","", WordforFilename)
write(EntryQMD, paste0("./MyDict/",WordforFilename,".qmd"))

}

MakeEntryForComboQMD <- function(Word, EntryData, TAB=NULL){

  Title <- paste("# ",Word, "{.unnumbered}")
  SetUp <-paste0('```{r  setUp_',SafeEncodeUTF8(Word),', include=FALSE}\n library(tidyverse) \n
      EntryData <- readRDS(paste0("./", dir("./")[str_detect(dir("./"), "', gsub(Word,".*?" ,MakeSafeForFilename(Word)),'.rds")]))',
                 '\n\n```')


  Contents <- lapply(seq_along(EntryData[1][[1]]), function(i) createEntryContent(dataDF=EntryData[1][[1]][i],  dataDFString=paste0('EntryData[1][[1]][',i,'][[1]]'),TAB) )

  Content <- paste(Contents, collapse="\n\n")

  EntryContent <- paste(Title,SetUp,Content,"{{< pagebreak >}}", sep="\n\n")
  #write(EntryContent, paste0("./MyDict/",Word,".qmd"))
  return(EntryContent)
}

MakeQmdByLetter <- function(HeadwordVec, initial, Dir="./MyDict/", TAB=NULL){
  HeadwordVec <- unique(HeadwordVec)
    LetterEntries <- unlist(lapply(HeadwordVec[str_detect(tolower(HeadwordVec),paste0("^",initial))], function(x)
    {print(x);if(file.exists(paste0(Dir,dir(Dir)[str_detect(dir(Dir),paste0("^Entry", MakeSafeForFilename(x), ".rds") )])) && length(dir(Dir)[str_detect(dir(Dir),paste0("^Entry", MakeSafeForFilename(x), ".rds") )])>0){

      MakeEntryForComboQMD(Word= x,
                           EntryData= readRDS(paste0(Dir,dir(Dir)[str_detect(dir(Dir),paste0("^Entry", MakeSafeForFilename(x), ".rds") )])),TAB)
    }}))


  Header <-
    '---
title: "INITIAL"
format: html
editor: visual
toc: true
---
'
  Header <- gsub("INITIAL",initial,Header)

  comboQmd <- paste(Header, paste(LetterEntries, collapse = "\n\n\n\n"), sep="\n\n")
  write(comboQmd, paste0(Dir,initial,".qmd"))
}


updateYaml <- function(QuartoDir){

  yamlFileLines <- readLines(paste0(QuartoDir, "/_quarto.yml"))
  LinesToRemove <- yamlFileLines[str_detect(yamlFileLines,"qmd$") & !str_detect(yamlFileLines,"index.qmd|references.qmd")]
  yamlFileLines <- yamlFileLines[!yamlFileLines %in% LinesToRemove]

  yamlFile <- paste(yamlFileLines, collapse="\n")
  qmdFiles <- unique(dir(QuartoDir)[str_detect(dir(QuartoDir), ".qmd$")])

  SpecialQmdFiles <- qmdFiles[c(which(qmdFiles=="index.qmd"), which(qmdFiles=="references.qmd"))]
  EntriesQmdFiles <- qmdFiles[-c(which(qmdFiles=="index.qmd"), which(qmdFiles=="references.qmd"))]

  qmdFilesstring <- paste(EntriesQmdFiles, collapse = "\n  - ")
  yamlFile <- gsub("references.qmd", paste("references.qmd", qmdFilesstring, sep = "\n  - " ), yamlFile)
  yamlFile <- gsub("\n  - \n", "\n", yamlFile)
  write(yamlFile, paste0(QuartoDir, "/_quarto.yml"))

}



ArrangeQmd <- function(Instructions,Dir,qmdFileName){

  if(str_detect(Instructions,"TAB\\(.*?\\)")){
    Qmd <- readtext::readtext(paste0(Dir,"/",qmdFileName))$text
    TABS <- unlist(str_extract_all(Instructions, "TAB\\(.*?\\)"))
    ItemsOrder <- unlist(str_split(gsub("TAB|\\(|\\)","",Instructions),";" ))

    for(tab in TABS){
      tab <- gsub("TAB|\\(|\\)","",tab)
      Items <- unlist(str_split(tab,";"))
      Qmd <- gsub(paste0("\n####(\\s+)?",Items[1],"(\\s+)?\n"), paste("\n::: panel-tabset\n","\n#### ",Items[1],"\n") , Qmd)
      Qmd <- gsub(paste0("\n####(\\s+)?", ItemsOrder[which(ItemsOrder==Items[length(Items)])+1],"(\\s+)?\n"), paste("\n:::\n","\n#### ", ItemsOrder[which(ItemsOrder==Items[length(Items)])+1],"\n") , Qmd)
    }
    write(Qmd,  paste0(Dir,"/",qmdFileName))
  }
}

ArrangeAllQmds <- function(Instructions,Dir){
  Instructions <- gsub("(\\s+)?;(\\s+)?", ";", Instructions)
  Instructions <- gsub(";\\)", ");", Instructions)
  if(str_detect(Instructions,"TAB\\(.*?\\)")){
     Files <-  dir(Dir)[str_detect(dir(Dir),"qmd$")]
     Files <- Files[!Files %in% c("index.qmd", "references.qmd")]
     if(length(Files)>0){
     lapply(Files, function(x) ArrangeQmd(Instructions,Dir,x) )
     }
}else{
  print("no TAB(...) instructions in input")
}

}


MakeAllentries <- function(HeadwordVec,RuleList, DictData, TAB=NULL, Cores){

  # mclapply(HeadwordVec, function(x) AssembleEntry(DictData, RuleList, x), mc.cores =Cores)
  #
  # mclapply(HeadwordVec, function(x)
  #          MakeQuartoFile(Word= x,
  #                         EntryData= readRDS(paste0("./MyDict/",dir("./MyDict")[str_detect(dir("./MyDict"),paste0("^Entry_xx", SafeEncodeUTF8(x), "xx_") )]))),
  #          mc.cores=Cores)

  # !! mclapply gives unexplained error here, temporarily switching to lapply

  lapply(HeadwordVec, function(x) {print(x);AssembleEntry(DictData, RuleList, x, projectType="book")})

  updateYaml("./MyDict")

  if(length(HeadwordVec)<1200){
  lapply(HeadwordVec, function(x) {print(x);MakeQuartoFile(Word= x,
                           EntryData= readRDS(paste0("./MyDict/",dir("./MyDict")[str_detect(dir("./MyDict"),paste0("^Entry", MakeSafeForFilename(x), ".rds") )])),
                           TAB)})

}else{

  # handle diacrititics in initial:
  Initials <- unique(gsub("[x_-]","",unlist(str_extract_all(dir("./MyDict")[str_detect(dir("./MyDict"), "^Entry")],"_.*?-" ))))
  Initials <- tolower(sort(unlist(lapply(Initials, function(x) SafeDecodeUTF8(x) ))))
  Initials <- unique(Initials)
  qmds <- dir("./MyDict")[str_detect(dir("./MyDict"), ".qmd$")]
  if(length(setdiff(qmds, c("index.qmd", "references.qmd")))>0){
  file.remove(paste0("./MyDict/",setdiff(qmds, c("index.qmd", "references.qmd"))))
  }
  if (Sys.info()[['sysname']]!="Windows"){
    mclapply(Initials, function(x) MakeQmdByLetter(HeadwordVec, x, "./MyDict/", TAB), mc.cores=Cores )
  }else{
  lapply(Initials, function(x) MakeQmdByLetter(HeadwordVec, x, "./MyDict/", TAB) )
 }
}


  updateYaml("./MyDict/")

  print("done!")

}


#MakeAllentries(c("saṃkalpa","vikalpa"),RuleList=readRDS("./DictMakingRuleList.rds"), Cores=2)

RenderDict <- function(){
  if(!str_detect(getwd(),"./MyDict$")){
  setwd("./MyDict")
  }
  quarto_render()
}

