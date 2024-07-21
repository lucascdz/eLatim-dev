#EntryData <- readRDS(paste0("./MyDict/data/", dir("./MyDict/data")[str_detect(dir("./MyDict/data"),"^Entry.*?.rds")][2]))
#EntryData <- readRDS(paste0("./MyDict/data/", dir("./MyDict/data")[str_detect(dir("./MyDict/data"),"^Entry.*?.rds")][1]))
# i <- 9
 # dataDF <- EntryData[1][[1]][i]
#RuleList <- readRDS("DictMakingRuleList.rds")
#Rules <- readRDS("DictMakingRuleList.rds")
# EntryData[1][[1]][1][[1]]
# EntryData[[1]][["frequency"]][[1]]
#rule<-RuleList[i]
#rule<-Rules$examples

#RuleList[9]$examples$Text <- "%%%filterby genre period"

cwebpMediaConverter <- function(headwords,filepaths, SourceDir, TargetDir='./MyDict/Media/',Cores){
  for(path in filepaths){
    if (Sys.info()[['sysname']]!="Windows"){
      mclapply(headwords, function(x) {webpfile <- paste0(TargetDir, gsub("^(.*?)_xx.*?xx_.*?(_.*?).png$", paste0("\\1",MakeSafeForFilename(x),"\\2.webp"),path)) ; if( !file.exists(webpfile) ){system(paste0('cwebp -q 80 ',SourceDir,path, ' -o ',webpfile ))}}, mc.cores=Cores)
    }else{
      lapply(headwords, function(x) {webpfile <- paste0(TargetDir, gsub("^(.*?)_xx.*?xx_.*?(_.*?).png$", paste0("\\1",MakeSafeForFilename(x),"\\2.webp"),path)) ; if( !file.exists(webpfile) ){system(paste0('cwebp -q 80 ',SourceDir,path, ' -o ',webpfile ))}})

    }
  }
}

MakeMediaRepoForShiny <- function(Rules, headwords,Cores){
  if(file.exists("./Media") && length(dir("./Media"))>0 && !file.exists("./MyDict/Media")){
    system("mkdir ./MyDict/Media")
  }

  Mediafiles <- lapply(names(Rules), function(x) if(str_detect(Rules[[x]]$Dataset,"^Media_") && T %in% str_detect(Rules[[x]]$Vars,"png$")){Rules[[x]]$Vars})
  Mediafiles <- do.call(c, Mediafiles)
  if(!is.null(Mediafiles) && file.exists("./Media")){
    cwebpMediaConverter(headwords, filepaths=Mediafiles, SourceDir='./Media',TargetDir='./MyDict/Media/',Cores)
  }

  imgs <- lapply(names(Rules), function(x) if(str_detect(Rules[[x]]$Dataset,"^Plots_") && T %in% str_detect(Rules[[x]]$Vars,"png$")){Rules[[x]]$Vars[str_detect(Rules[[x]]$Vars,"png$")]})
  imgs <- do.call(c, imgs)

  if(!is.null(imgs)){
    cwebpMediaConverter(headwords, filepaths=imgs, SourceDir='./data/Plots/',TargetDir='./MyDict/Media/',Cores)
  }

  if(file.exists("./data/AltText") && length(dir("./data/AltText"))>0 ){
    if(file.exists("./MyDict/AltText")){
      system("rm -r ./MyDict/AltText")
    }
    if (Sys.info()[['sysname']]!="Windows"){
      system('cp -r ./data/AltText ./MyDict/AltText')
    }else{
      system('powershell.exe', input= 'cp -r ./data/AltText ./MyDict/AltText', intern=T)
    }
  }


}


AddFilter <- function(rule, ElementName){

  # this is designed to work with Examples.
  # it requires a DF with a column called either Sent or ResultString, with the examples
  # AND a column named ID, which should have the reference for the example.

  filterVars <- unlist(str_extract(rule$Text, "%%%filterby .*?($|%%%)") %>%
                         str_split(.," "))
  filterVars <- filterVars[!str_detect(filterVars,"%%%")]
  Vars <- rule$Vars[!rule$Vars %in% filterVars]
  if(length(filterVars)>0){

    # just one meny filter with filter by filterVars
    UIcontent <- paste0(" selectInput('filterby_",ElementName,"', 'filter by', choices=c('", paste(c('no filter', filterVars), collapse = "','"),"')),
                          selectInput('filterVar_",ElementName,"', 'select', choices=NULL),
                          sliderInput('slider_",ElementName,"', 'number of examples' , min=1, max=1,value=1,step=1),
                          htmlOutput('", ElementName ,"'),")

    ServerContent <- paste0("
    observeEvent(input$filterby_",ElementName," ,{
  if(input$filterby_",ElementName,"!='no filter'){
  updateSelectInput(session, 'filterVar_",ElementName,"', choices=c('no filter',unique(EntryData[[1]][['",ElementName,"']][[input$filterby_",ElementName,"]])))
  }else{
  updateSelectInput(session, 'filterVar_",ElementName,"', choices='no filter')
  }

  observeEvent(input$filterVar_",ElementName," ,{
  if(input$filterVar_",ElementName,"=='no filter'){
  updateSliderInput(session, 'slider_",ElementName,"', min=1, max=nrow(EntryData[[1]][['",ElementName,"']]),value=ceiling(nrow(EntryData[[1]][['",ElementName,"']])/2))
  }else{
  RowNo <- nrow(EntryData[[1]][['",ElementName,"']][which(EntryData[[1]][['",ElementName,"']][[input$filterby_",ElementName,"]]==input$filterVar_",ElementName,"),])
    updateSliderInput(session, 'slider_",ElementName,"', min=1, max=RowNo, value=ceiling(RowNo/2))
  }

  observeEvent(input$slider_",ElementName,",{
    output$",ElementName," <- renderText({
      if(input$filterVar_",ElementName,"!='no filter'){
        DF <- EntryData[[1]][['",ElementName,"']][which(EntryData[[1]][['",ElementName,"']][[input$filterby_",ElementName,"]]==input$filterVar_",ElementName,"),]
      }else{
        DF <- EntryData[[1]][['",ElementName,"']]
      }
      if(nrow(DF)>0){
      DF <- DF[1:input$slider_",ElementName,",]
      if('ResultString' %in% colnames(DF)){
           paste(DF$ResultString, collapse='<br /><br />')

      }else if('Sent' %in% colnames(DF)){
          paste(paste0(DF$Sent, ' [',DF$ID, ']'), collapse='<br /><br />')

      }else{
      paste(apply( DF, 1 , paste , collapse = " , " ), collapse = '<br /><br />')
      }
      }else{
      paste('no data')
      }
    })
   })
  })
 })
 ")
    return(list(UI=UIcontent,server=ServerContent ))
  }

}

GetCOLSorTABS <- function(InstructionsFilePath, COLorTAB="COL"){
  if(file.exists(InstructionsFilePath)){
    Instructions <- readtext(InstructionsFilePath)$text
    if(str_detect(Instructions, paste0(COLorTAB,"\\(.*?\\)"))){
      COLSorTABS <- unlist(str_extract_all(Instructions, paste0(COLorTAB,"\\(.*?\\)")))
      COLSorTABS <- lapply(COLSorTABS, function(x) unlist(str_split(gsub(paste0(COLorTAB,"|\\(|\\)"),"",x),";")))
    }else{
      COLSorTABS <- NULL
    }
  }else{
    COLSorTABS <- NULL
  }
  return(COLSorTABS)
}

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

addColstoUI <- function(colset){
  uiLines <- readLines("./UI.txt")

  for(i in seq_along(colset)){

    uiLines[str_detect(uiLines, paste0("Output\\('title_",gsub("\\s+","", colset[i]),"'\\)"))] <- paste0(
      "column(width=", floor(9/length(colset)),",
       ",
      uiLines[str_detect(uiLines, paste0("Output\\('title_",gsub("\\s+","", colset[i]),"'\\)"))]
    )

    uiLines[str_detect(uiLines, paste0("\\('", gsub("\\s+","", colset[i]),"'\\)" ))] <- paste0(
      uiLines[str_detect(uiLines, paste0("\\('", gsub("\\s+","", colset[i]),"'\\)" ))],
      "
    ),"
    )

  }

  uiLines[str_detect(uiLines, paste0("Output\\('title_",colset[1],"'\\)"))] <- paste0(
    "fluidRow(
       ",
    uiLines[str_detect(uiLines, paste0("Output\\('title_",colset[1],"'\\)"))]
  )

  uiLines[str_detect(uiLines, paste0("Output\\('",gsub("\\s+","",colset[length(colset)]),"'\\)"))] <- paste0(
    uiLines[str_detect(uiLines, paste0("Output\\('",gsub("\\s+","",colset[length(colset)]),"'\\)"))],
    "),"
  )

  colsetStart <-which(str_detect(uiLines, paste0("Output\\('title_",colset[1],"'\\)")))
  colsetEnd <-which(str_detect(uiLines, paste0("Output\\('",gsub("\\s+","",colset[length(colset)]),"'\\)")))
  toRemove <- intersect(which(uiLines=="tags$br(),"),colsetStart:colsetEnd)
  if(length(toRemove)>0){
    uiLines <- uiLines[-toRemove]
  }
  write(paste(uiLines, collapse = "\n"),"./UI.txt")
}

addTabstoUI <- function(tabset){
  uiLines <- readLines("./UI.txt")
for(t in tabset){
  print(t)
  uiLines[str_detect(uiLines, paste0("Output\\('title_",gsub("\\s+","", t),"'\\)"))] <- paste0(
    "tabPanel('",t,"',
       ",
    uiLines[str_detect(uiLines, paste0("Output\\('title_",gsub("\\s+","", t),"'\\)"))]
  )

  uiLines[str_detect(uiLines, paste0("Output\\('",gsub("\\s+","", t),"'\\)"))] <- paste0(

    uiLines[str_detect(uiLines, paste0("Output\\('",gsub("\\s+","", t),"'\\)"))],"
    ),
    "
  )

}
  # uiLines[str_detect(uiLines, paste0("\\('", gsub("\\s+","", tabset[length(tabset)]),"'\\)" ))] <- paste0(
  #   uiLines[str_detect(uiLines, paste0("\\('", gsub("\\s+","", tabset[length(tabset)]),"'\\)" ))],
  #   "
  # ),"
  # )

  write(paste(uiLines, collapse = "\n"),"./UI.txt")
}

addUIcols <- function(COLs){

  UIfile <- readtext::readtext("./UI.txt")$text

  for(colset in COLs){

    addColstoUI(colset)

  }
}

addUItabs <- function(TABs){

  UIfile <- readtext::readtext("./UI.txt")$text

  for(tabset in TABs){
    addTabstoUI(tabset)
  }

  uiLines <- readLines("./UI.txt")
  uiLines[str_detect(uiLines, fixed(paste0("tabPanel('",TABs[1][[1]][[1]],"'")))] <- paste0(
    "tabsetPanel(
       ",
    uiLines[str_detect(uiLines, fixed(paste0("tabPanel('",TABs[1][[1]][[1]],"'")))]
  )

  uiLines[str_detect(uiLines, paste0("Output\\('",gsub("\\s+","",TABs[length(TABs)][[1]][[length(TABs[length(TABs)][[1]])]]
),"'\\)"))] <- paste0(
    uiLines[str_detect(uiLines, paste0("Output\\('",gsub("\\s+","",TABs[length(TABs)][[1]][[length(TABs[length(TABs)][[1]])]]
),"'\\)"))],
    "),"
  )
  uiLines <- uiLines[uiLines!=""]
  write(paste(uiLines, collapse = "\n"),"./UI.txt")

}


GetDFfromRDSlistElements <- function(filename, listElements){
  #print(filename)
  entry <- readRDS(paste0("./MyDict/data/",filename))
  #DF <- lapply(listElements, function(x) {DF <- data.frame(filename, names(entry),  ifelse(!is.null(entry[[1]][[x]][[length(entry[[1]][[x]])]]), entry[[1]][[x]][[length(entry[[1]][[x]])]],"")); colnames(DF) <- c("file", x); DF})
  DF <- lapply(listElements, function(x) {DF <- data.frame(filename, ifelse(!is.null(entry[[1]][[x]][[length(entry[[1]][[x]])]]) & length(entry[[1]][[x]][[length(entry[[1]][[x]])]])>0, entry[[1]][[x]][[length(entry[[1]][[x]])]],"")); colnames(DF) <- c("file", x); DF})
  DF <- do.call(cbind, DF)
  return(DF)
}

prepData <- function(Rules, headwords, Cores){
  MakeMediaRepoForShiny(Rules, headwords, Cores)
  # getHeadwords from files and treat diacritics
  entryfiles <- dir("./MyDict/data")[str_detect(dir("./MyDict/data"),"^Entry_\\d")]

  if(file.exists("./Instructions.txt") && str_detect(readtext("./Instructions.txt")$text,"SEARCHBY\\(.*?\\)")){
    searchBy <- unlist(str_extract(readtext("./Instructions.txt")$text,"SEARCHBY\\(.*?\\)"))%>%
      str_remove_all("SEARCHBY\\(|\\)")%>%
      str_split(";")%>%
      unlist(.)

    searchDF <- lapply(entryfiles, function(x)  {print(x);GetDFfromRDSlistElements(x, searchBy)} )
    headwords <- do.call(rbind, searchDF)
    if(length(setdiff(searchBy, names(Rules)))){
      print(paste(setdiff(searchBy, names(Rules)), "is not in your data, please check Instructions file"))
    }
    for( col in colnames(headwords)[colnames(headwords)!="file"]){
      if(file.exists("./data/DiacriticsConversion.csv")){
        newcol <- ConvertDiacritics(read.csv("./data/DiacriticsConversion.csv",stringsAsFactors = F), gsub("[^A-z]"," ",headwords[[col]]))
      }else{
        newcol <- tolower(headwords[[col]])
        newcol <- gsub("[^A-z]"," ",newcol)
      }
      headwords <- cbind(headwords,newcol)
      colnames(headwords)[length(headwords)] <- paste0(col, "NoDiacritics")
      headwords
    }

  }else{

    headwords <- lapply(entryfiles, function(x)  data.frame(file=x, headwords=SafeDecodeUTF8(gsub("^Entry_|.rds$","",x))))
    headwords <- do.call(rbind, headwords)
    if(file.exists("./data/DiacriticsConversion.csv")){
      headwords$headwordsNoDiacritics <- ConvertDiacritics(read.csv("./data/DiacriticsConversion.csv",stringsAsFactors = F), gsub("\\d+","",headwords$headwords))
      headwords$headwordsNoDiacritics <- gsub("[^A-z]" ," ",headwords$headwordsNoDiacritics)
      }else{
        headwords$headwordsNoDiacritics <- tolower(headwords$headwords)
        headwords$headwordsNoDiacritics <- gsub("[^A-z]" ," ",headwords$headwordsNoDiacritics)

      }
    #headwords
  }

  write.fst(headwords, "./MyDict/data/headwords.fst")

  if(!file.exists("./MyDict/Home.Rmd")){
    Home <-"---
title: 'Home'
---
 <br />
 <br />
 <br />
 <br />

 This resource was made available in digital form thanks to the NEH-funded project Democratizing Digital Lexicography (HAA-290402-23)."
  }else{
    Home <- readtext("./MyDict/Home.Rmd")$text
    if(!str_detect(Home, "HAA-290402-23")){
    NEHcredit <- "This resource was made available in digital form thanks to the NEH-funded project Democratizing Digital Lexicography (HAA-290402-23)."
    Home <-  paste(Home, NEHcredit, sep="\n\n")
    }
    if(file.exists("./cover.webp")){
      Home <-  paste("![](cover.webp)",Home,sep="\n\n")
    }
  }
  write(Home, "./MyDict/Home.Rmd")

  write("","./UI.txt")
  write("","./SERVER.txt")
}


createShinyEntry <- function(dataDF=EntryData[1][[1]][i], rule=RuleList[i]){


  UIfile <- readtext::readtext("./UI.txt")$text
  ServerFile <- readtext::readtext("./SERVER.txt")$text

  # make UI element
  ElementName <- gsub("[^A-z]","",names(dataDF))

  UItitle <- paste0("htmlOutput('",paste0("title_", ElementName),"'),")
  ServerTitle <- paste0("output$",paste0("title_", ElementName)," <- renderText({
          '<font size=\\'+2\\' color=\\'steelblue\\'>",names(dataDF),"</font>'
                           })")

  if(!is.null(dataDF[[1]])){

  if(is.data.frame(dataDF[[1]])){

    if(!T %in% str_detect(rule[[1]]$Text,"%%%filterby ")){

    UIcontent <- paste0("DT::dataTableOutput('", ElementName ,"'),")
    ServerContent <- paste0("output$",ElementName," <- DT::renderDataTable(
          data.table(EntryData[[1]][['",names(dataDF),"']])
        )")
    }else{
      content <- AddFilter(rule[[1]], ElementName)
      UIcontent <- content$UI
      ServerContent <- content$server
    }
  }else if(T %in% str_detect(dataDF[[1]],"webp|png")){

    UIcontent <- paste0("htmlOutput('", ElementName ,"'),")
    # ServerContent <- paste0("output$",ElementName," <- renderText({
    # paste0(\"<img src='tmpuser/\",gsub(\"\\\\./MyDict/Media/\",\"\",as.character(EntryData[[1]][['",names(dataDF),"']][[1]])),\"', width=50%>\")
    #                        })")

    ServerContent <- paste0("output$",ElementName," <- renderText({
  if(str_detect(names(EntryData),'[^a-z]')){
  Files <- dir('./Media')[str_detect(dir('./Media'), SafeEncodeUTF8(names(EntryData)))]
  Pattern <- gsub('\\\\./MyDict/Media/','',gsub('xx_.*?_','xx_.*?_',as.character(EntryData[[1]][['",names(dataDF),"']][[1]])))
  file <- Files[str_detect(Files, Pattern)]
  paste0(\"<img alt='\", readtext(paste0('./AltText/', gsub('.webp$','_AltText.txt', file)))$text,\"' src='tmpuser/\",file,\"', width=50%>\")
   }else{
    paste0(\"<img alt='\", readtext(str_replace(EntryData[[1]][['",names(dataDF),"']][[1]], './MyDict/Media/','./AltText/') %>%
           str_replace(.,'.webp','_AltText.txt'))$text,\"' src='tmpuser/\",gsub(\"\\\\./MyDict/Media/\",\"\",as.character(EntryData[[1]][['",names(dataDF),"']][[1]])),\"', width=50%>\")
   }

                           })")

  }else{

    # add here conditionals for html/audio/video ...

    UIcontent <- paste0("htmlOutput('", ElementName ,"'),")
    # ServerContent <- paste0("output$",ElementName," <- renderText({
    #       gsub('\n','<br />',EntryData[[1]][['",names(dataDF),"']][[1]])
    #                        })")
    ServerContent <- paste0(    "output$",ElementName," <- renderText({
        paste0(\"<font size='+1'>\",
          gsub('\n','<br />',EntryData[[1]][['",names(dataDF),"']][[1]]),
          \"</font>\"
                         )  })")
  }
  }else{
    UIcontent <- paste0("htmlOutput('", ElementName ,"'),")
    ServerContent <- paste0("output$",ElementName," <- renderText({
         'no data'
                           })")
  }

write(paste(UIfile,"tags$br(),","tags$br(),","tags$br(),",UItitle,UIcontent, sep="\n\n"), "./UI.txt")
write(paste(ServerFile,ServerTitle,ServerContent, sep="\n\n"), "./SERVER.txt")

}


MakeShinyScript <- function(HeadwordVec, COLS,TABS){

  Credits <- readtext("./MyDict/DictCredits.txt")$text
  dictTitle <- str_extract(Credits, "title=.*?author") %>%
                            str_remove_all("title=|author")
  headwordsDF <- read.fst("./MyDict/data/headwords.fst")
  noDiaIndex <- grep("NoDiacritics$",colnames(headwordsDF))

  if(file.exists("./previewWord.txt")){
    previewWord <- readtext("./previewWord.txt")$text
  }else{
    previewWord <- HeadwordVec[1]
  }
  if(!is.null(TABS)){
    addUItabs(TABS)
  }

  if(!is.null(COLS)){
    addUIcols(COLS)
  }
  UIfile <- readtext::readtext("./UI.txt")$text

  ServerFile <- readtext::readtext("./SERVER.txt")$text


  BlockOne <-paste0('library(shiny)
  library(stringr)
  library(readtext)
  library(fst)
  library(DT)
  library(data.table)
  SafeEncodeUTF8 <- function(string){
  paste(utf8ToInt(string), collapse = "-")
  }

  ui <- fluidPage(
    titlePanel("Mock Dict"),
        actionButton("stop","stop preview"),
        tabsetPanel(
    tabPanel("Home",
           includeMarkdown("Home.Rmd")
    ),
    tabPanel("Dictionary",
    sidebarLayout(
        sidebarPanel(width=3,
          selectInput("searchBy", "search by", choices = NULL),

          textInput("headwordText",
                           "input headword",
                            value="',previewWord,'"),
          #actionBttn("go","go"),
          selectInput("headword", "filter matches", choices = "no matches")

        ),
        mainPanel(width=9,
  ')


    BlockTwo <- '
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                )
               )
             ),
             '

    Rmd <- dir("./MyDict")[str_detect(dir("./MyDict"), "Rmd$")]
    Rmd <- Rmd[Rmd!="Home.Rmd"]

    AdditionalTabs <- ""
    for(i in Rmd){
      AdditionalTabs <- paste0(AdditionalTabs,'
      tabPanel("',gsub(".Rmd$","",i) ,'",
           fluidRow(
             column(width=3,""),
             column(width=6,
                    includeMarkdown("',i,'")
             ),
             column(width=3,""),
         )
    ),
    ')
    }

         BlockThree <- '
         )
         )

       server <- function(input, output,session) {

              if(file.exists("./Media/")){
              addResourcePath(prefix = "tmpuser", directoryPath = "./Media/")
              addResourcePath(prefix = "AltText", directoryPath = "./AltText/")

              }
            headwords <- read.fst("./data/headwords.fst")

            searchOptions <- colnames(headwords)
            searchOptions <- searchOptions[!str_detect(searchOptions, "^file$|NoDiacritics$")]

             updateSelectInput(session, "searchBy", choices=searchOptions)

            observeEvent(c(input$searchBy, input$headwordText),{


         if(!is.null(input$headwordText) && nchar(input$headwordText)>0){
                 CHOICES <- c(headwords[[input$searchBy]][grep(paste0("(^|\\\\s)",input$headwordText,"(\\\\s|$)"),headwords[[paste0(input$searchBy,"NoDiacritics")]], ignore.case=T)],
                           headwords[[input$searchBy]][grep(paste0("(^|\\\\s)",input$headwordText,"(\\\\s|$)"),headwords[[input$searchBy]])])
              }else{
                CHOICES <- "no matches"
              }

              if(length(unique(CHOICES))<length(CHOICES)){
                CHOICES <- unlist(lapply(unique(CHOICES), function(x) paste(x, 1:length(headwords$file[headwords[[input$searchBy]]==x]), sep="__") ))
              }else{
                CHOICES <- unique(CHOICES)
              }
             updateSelectInput(session, "headword", choices=CHOICES )

            })


          observeEvent(c(input$headword,input$searchBy),{
          headIndex <- unlist(str_split(input$headword, "__"))
              if(headIndex[1]!="no matches" && length(headwords$file[headwords[[input$searchBy]]==headIndex[1]])>0){
                if(length(headIndex)==1){
                  EntryData <- readRDS(paste0("./data/", headwords$file[headwords[[input$searchBy]]==headIndex[1]]))
                }else{
                  EntryData <- readRDS(paste0("./data/", headwords$file[headwords[[input$searchBy]]==headIndex[1]][as.numeric(headIndex[2])]))
                }
                       }else{
            EntryData <- NULL
            print(paste("no data matching ",input$headword))
            }
          if(!is.null(EntryData)){
 '

  #}


  BlockFour <- '}
               })
          observeEvent(input$stop,{
            stopApp()
          })
               }
               shinyApp(ui = ui, server = server)
'


  BlockOne <- gsub('titlePanel\\("Mock Dict"\\)',paste0('titlePanel("', dictTitle,'")') ,BlockOne)
  AppScript <- paste(BlockOne, UIfile,BlockTwo, AdditionalTabs, BlockThree,ServerFile,BlockFour)


  write(AppScript,"./MyDict/app.R")

}

WriteShinyApp <- function(entryHeadword, HeadwordVec, Cores){


EntryData <- readRDS(paste0("./MyDict/data/Entry_", SafeEncodeUTF8(entryHeadword),".rds"))
RuleList <- readRDS("DictMakingRuleList.rds")

prepData(RuleList, HeadwordVec, Cores)

for (i in seq_along(EntryData[1][[1]])){

  createShinyEntry(dataDF=EntryData[1][[1]][i],rule=RuleList[i])
}

MakeShinyScript(HeadwordVec, COLS=GetCOLSorTABS("./Instructions.txt","COL"), TABS=GetCOLSorTABS("./Instructions.txt","TAB"))
}



ExportShinyDict <- function(){

  appScript <- readtext("./MyDict/app.R")$text
  appScript <- gsub('actionButton\\("stop","stop preview"\\)','# actionButton("stop","stop preview")',appScript)
  write(appScript, "./MyDict/app.R")

  print('the MyDict subfolder contains your web-ready shiny dictionary')
}


