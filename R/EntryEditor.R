EntryEditorUI <- function(id){

  tagList(
    column(width = 4,
           # view rendered dictionary & entry-making rules:
           actionButton(NS(id,"preview"),"preview dict"),
           actionButton(NS(id, "refreshRules"), "refresh rules"),
           tags$hr(),
           # create entry through rules:
           tags$div(HTML("<font size='+1'> create a template for your entries</font>")),
           textInput(NS(id, "Word"),"input a headword to test on", value=NULL),
           tags$div(HTML("<font size='+1'>select data to include in entry</font>")),
           textInput(NS(id, "ElementName"),"Name for data-element", ""),
           selectInput(NS(id,"DataSource"),"select data source", choices = NULL),
           checkboxGroupInput(NS(id,"DataVar"),"select variable from data", choices = NULL),
           textAreaInput(NS(id, "Text"),"create text (see documentation)", ""),
           actionBttn(NS(id,"go"), "go"),
           tags$br(),
           tags$hr(),
           # edit rules and entry interface
           tags$div(HTML("<font size='+1' color='indianred'>modify your entry-making rules</font>")),
           tags$br(),
           selectInput(NS(id,"modify"),"section to delete", choices = NULL),
           tags$br(),
           actionButton(NS(id, "delete"), "delete section"),
           tags$br(),
           textAreaInput(NS(id, "rearrange"),"rearrange entry sections (see documentation)", value=NULL),
           checkboxInput(NS(id, "rearrangeGo"), "re-order", value=F),
           #uiOutput(NS(id,"rearrangeMod"))
           textAreaInput(NS(id,"addTabs"), "add UI instructions (see documentation)"),
           checkboxInput(NS(id,"addTabsGo"), "update UI instructions", value=F),
           # add easy way to create inter-links?
           tags$br(),
           tags$br(),
           actionBttn(NS(id,"doAllEntries"), "process all entries"),
           tags$br(),
           tags$br(),
           tags$br(),
           tags$br(),
           actionBttn(NS(id,"renderDict"), "create digital dictionary"),
           tags$br(),
           tags$br(),
           tags$br(),
           tags$br(),

    ),
    column(width = 8,
           # tabsetPanel(
           #   tabPanel("dictionary",
           #            uiOutput(NS(id,"renderedHTML"))
           #   ),
           #   tabPanel("rules",
                      htmlOutput(NS(id,"RuleList"))
            # )
           #)
    )
  )
}


EntryEditorServer <- function(id, DictData, Cores){
  moduleServer(id, function(input, output, session) {

    # sanitize names for filenaming
     names(DictData) <- str_replace_all(names(DictData), "[^[[:lower:]][[:upper:]]\\d\\s-{}]"," ")
     names(DictData) <-  gsub("^\\s+|\\s+$","", names(DictData))

if(file.exists("MyDict/_book")){
    addResourcePath(prefix = "rendered", directoryPath = "MyDict/_book")
}
    observeEvent(input$preview,{

      if(file.exists("./DictMakingRuleList.rds")){
      if(length(dir("./MyDict")[str_detect(dir("./MyDict"),"qmd$")])<3){
        if(file.exists("./data/dictData.rds")){
        # Word <- str_replace_all(names(DictData)[1], "[^[[:lower:]][[:upper:]]\\d\\s-{}]"," ")
        # Word <-  gsub("^\\s+|\\s+$","", Word)
          Word <- names(DictData)[1]
        }else if(length(dir("./MyDict")[str_detect(dir("./MyDict"),"^Entry_xx")])>0){
          WordVec <- unlist(lapply(dir("./MyDict")[str_detect(dir("./MyDict"),"^Entry_xx")], function(x) DecodeSafeFilename(x)))
          # Word <- str_replace_all(WordVec[1], "[^[[:lower:]][[:upper:]]\\d\\s-{}]"," ")
          # Word <-  gsub("^\\s+|\\s+$","", Word)
          Word <- WordVec[1]
        }else{
          print("no dictionary data, upload dictioanry data or headwords file to continue")
        }

        FirstEntryPath <- paste0("./MyDict/",dir("./MyDict")[str_detect(dir("./MyDict"),paste0("^Entry",MakeSafeForFilename(Word),".rds"))])

        if(FirstEntryPath=="./MyDict/"){
          AssembleEntry(DictData, readRDS("./DictMakingRuleList.rds"), Word, projectType="book")
          FirstEntryPath <- paste0("./MyDict/",dir("./MyDict")[str_detect(dir("./MyDict"),paste0("^Entry",MakeSafeForFilename(Word),".rds"))])
        }

        MakeQuartoFile(Word, readRDS(FirstEntryPath),TAB= GetTABS("./TabsInstructions.txt"))
        updateYaml("./MyDict")

      }else{
        WordVec <- unlist(lapply(dir("./MyDict")[str_detect(dir("./MyDict"),"^Entry_xx")], function(x) DecodeSafeFilename(x)))
        MakeAllentries(HeadwordVec= WordVec,
                      RuleList= readRDS("./DictMakingRuleList.rds"),
                      DictData= DictData,
                      TAB = GetTABS("./TabsInstructions.txt"),
                      Cores= Cores)

      }

      if(!str_detect(getwd(),"/MyDict$")){
        setwd("./MyDict/")
      }

      quarto_preview_stop()
      #getOption("viewer", utils::browseURL)
      quarto_preview()
      if(str_detect(getwd(),"/MyDict$")){
      setwd("../")
      }


      if(file.exists("./MyDict/_book/index.html")){
        system("open ./MyDict/_book/index.html")
        output$renderedHTML <- renderUI({
          tags$iframe(style="height:485px; width:100%", src = "rendered/index.html")
        })
      }
      }else{
        print("no entry-making rules found")
      }
    })


    if(length(dir("./MyDict")[str_detect(dir("./MyDict"), "^Entry_")])>0){
      Word <- SafeDecodeUTF8(gsub("[x|_]","", unlist(str_extract(dir("./MyDict")[str_detect(dir("./MyDict"), "^Entry_")][1], "_xx.*?xx_"))))
    }else{
      Word <- ""
    }

    updateTextInput(session, "Word",
                    value=Word)



    AvailableData <- GetAvailableData(DictData)


    updateSelectInput(session, "DataSource",
                      choices=AvailableData, selected=AvailableData[1])

    AvailableVars <- c()


    observeEvent(c(input$DataSource, input$Word), {


      if(!is.null(input$Word) && nchar(input$Word)>0 && nchar(input$DataSource)>0){

       Word <- str_replace_all(input$Word, "[^[[:lower:]][[:upper:]]\\d\\s-{}]"," ")
       Word <-  gsub("^\\s+|\\s+$","", Word)

        updateCheckboxGroupInput(session, "DataVar",
                                 choices=GetAvailableVars(input$DataSource, DictData, Word))


      }
    })

    observeEvent(input$go, {

      if(!file.exists("./DictMakingRuleList.rds")){
        RuleList <- list()
      }else{
        RuleList <- readRDS("./DictMakingRuleList.rds")
      }

      if(nchar(input$ElementName)>0 && nchar(input$DataSource)>0 && !is.null(input$DataVar) && sum(nchar(input$DataVar)) >0){

        if(length(intersect(input$ElementName, names(RuleList)))==0){
        NewRuleList <-  list(list(ElementName = input$ElementName , Dataset=input$DataSource, Vars= input$DataVar, Text= input$Text))
        names(NewRuleList) <- NewRuleList[[1]]$ElementName
        RuleList <- append(RuleList,NewRuleList)
        saveRDS(RuleList,"./DictMakingRuleList.rds")
        RuleListJson <- jsonlite::toJSON(RuleList, pretty = T)
        write(RuleListJson, "./DictMakingRuleList.json")
        }else{
          print("a section with this name already exists, please use another name")
        }

      }else{
        print("an input is missing: have you inputted section name and selected a variable? if you get nothing to select in the variable field, your chosen data source is not available for your chosen word")
      }

    # Word <- str_replace_all(input$Word, "[^[[:lower:]][[:upper:]]\\d\\s-{}]"," ")
    # Word <-  gsub("^\\s+|\\s+$","", Word)
      Word <- input$Word
      AssembleEntry(DictData, RuleList, Word, projectType="book")

      if(length(dir("./MyDict")[str_detect(dir("./MyDict"), "^Entry_")])>0){

        MakeQuartoFile(Word, readRDS(paste0("./MyDict/",dir("./MyDict")[str_detect(dir("./MyDict"), paste0("^Entry_xx",SafeEncodeUTF8(Word)))])),TAB= GetTABS("./TabsInstructions.txt"))

        updateYaml("./MyDict")
      }
    })

    observeEvent(input$refreshRules,{

      if(file.exists("./DictMakingRuleList.rds")){

        RuleList <- readRDS("./DictMakingRuleList.rds")
        RuleListJson <- jsonlite::toJSON(RuleList, pretty = T)
        RuleNames <- names(RuleList)
  output$RuleList <- renderText({
    RuleListJson <- gsub("\\}","<br />}", RuleListJson)
          paste(gsub("(\\{|\\},|\\],)","\\1<br />", RuleListJson))
        })
      }else{
        RuleList <- NULL
        RuleNames <- ""
      }

      updateSelectInput(session, "modify",
                        choices =  RuleNames)

      updateTextAreaInput(session, "rearrange",
                      value=paste(RuleNames, collapse=";") )

      # output$rearrangeMod <- renderUI({
      #   rearrangeUI(NS(id,"Rearrange"),RuleList)
      # })
      #
      # rearrangeServer(id="Rearrange", RuleList,"./DictMakingRuleList.rds")
      #

})

    observeEvent(input$delete,{
      # Word <- str_replace_all(input$Word, "[^[[:lower:]][[:upper:]]\\d\\s-{}]"," ")
      # Word <-  gsub("^\\s+|\\s+$","", Word)
      Word <- input$Word
      if(file.exists("./DictMakingRuleList.rds")){

      RuleList <- readRDS("./DictMakingRuleList.rds")
      RuleList <-  RuleList[names(RuleList)!=input$modify]
      saveRDS(RuleList,"./DictMakingRuleList.rds")
      RuleListJson <- jsonlite::toJSON(RuleList, pretty = T)
      write(RuleListJson, "./DictMakingRuleList.json")
      AssembleEntry(DictData, RuleList, Word, projectType="book")
      MakeQuartoFile(Word, readRDS(paste0("./MyDict/",dir("./MyDict")[str_detect(dir("./MyDict"), paste0("^Entry_xx",SafeEncodeUTF8(Word)))])),TAB= GetTABS("./TabsInstructions.txt"))
      }
    })

    observeEvent(c(input$rearrange, input$rearrangeGo),{
if(input$rearrangeGo==T){

      if(file.exists("./DictMakingRuleList.rds")){

      RuleList <- readRDS("./DictMakingRuleList.rds")
      Input <- gsub("(\\s+)?;(\\s+)?",";",input$rearrange)
      NewOrder <- unlist(str_split(Input,";"))
      NewOrder <- intersect(NewOrder, names(RuleList))
      Word <- str_replace_all(input$Word, "[^[[:lower:]][[:upper:]]\\d\\s-{}]"," ")
      Word <-  gsub("^\\s+|\\s+$","", Word)
      if(length(NewOrder)==length(RuleList)){

      RuleList <-  RuleList[NewOrder]
      saveRDS(RuleList,"./DictMakingRuleList.rds")
      RuleListJson <- jsonlite::toJSON(RuleList, pretty = T)
      write(RuleListJson, "./DictMakingRuleList.json")
      AssembleEntry(DictData, RuleList, Word, projectType="book")
      MakeQuartoFile(Word, readRDS(paste0("./MyDict/",dir("./MyDict")[str_detect(dir("./MyDict"), paste0("^Entry_xx",SafeEncodeUTF8(Word)))])),TAB= GetTABS("./TabsInstructions.txt"))
      }else{
        print(paste("the following sections are not accounted for:", paste(setdiff(names(RuleList), NewOrder), collcapse=";")))
      }

      }
  updateCheckboxInput(session, "rearrangeGo",value=F)
}
    })
      observeEvent(c(input$addTabs,input$addTabsGo),{
   if(input$addTabsGo==T){

     write(as.character(input$addTabs), "./TabsInstructions.txt")
     updateCheckboxInput(session, "addTabsGo",value=F)
   }
    })

    observeEvent(input$doAllEntries,{


      HeadwordVec <- c()
      if(file.exists("./data/dictData.rds")){
        HeadwordVec <- names(readRDS("./data/dictData.rds"))
      }
      if(file.exists("./data/dictData.csv")){
        HeadwordVec <- unique(c(HeadwordVec, read.csv("./data/dictData.csv",stringsAsFactors = F)[,1]))
      }
      HeadwordVec <- HeadwordVec[nchar(HeadwordVec)<60]  # avoids error with too long filenames
      HeadwordVec <- str_replace_all(HeadwordVec, "[^[[:lower:]][[:upper:]]\\d\\s-{}]"," ")
      HeadwordVec <-  gsub("^\\s+|\\s+$","", HeadwordVec)

      MakeAllentries(HeadwordVec= unique(HeadwordVec),
                     RuleList= readRDS("./DictMakingRuleList.rds"),
                     DictData= DictData,
                     TAB = GetTABS("./TabsInstructions.txt"),
                     Cores= Cores)

      #MakeSharableDictData()

      showModal(
        modalDialog(
          title = "processing",
          "look at RStudio Background Jobs tab to monitor porgress",
          easyClose = TRUE,
          footer = NULL
        )
      )

    })

    observeEvent(input$renderDict,{

      RenderDict()
      showModal(
        modalDialog(
          title = "processing",
          "look at RStudio Background Jobs tab to monitor porgress",
          easyClose = TRUE,
          footer = NULL
        )
      )

    })

  })
}







