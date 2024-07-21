EntryEditorServerSHINY <- function(id, DictData, Cores){
  moduleServer(id, function(input, output, session) {


    HeadwordVec <- c()
    if(file.exists("./data/dictData.rds")){
      HeadwordVec <- names(readRDS("./data/dictData.rds"))
    }
    if(file.exists("./data/dictData.csv")){
      HeadwordVec <- unique(c(HeadwordVec, read.csv("./data/dictData.csv",stringsAsFactors = F)[,1]))
    }

    HeadwordVec <- HeadwordVec[nchar(HeadwordVec)<60] # avoids error with too long filenames

    observeEvent(input$preview,{

      if(file.exists("./DictMakingRuleList.rds")){
        RuleList <- readRDS("./DictMakingRuleList.rds")

if(file.exists("./previewWord.txt")){
  previewWord <- readtext("./previewWord.txt")$text
  previewVec <- c(HeadwordVec[1:3],previewWord)
}else{
  previewWord <- HeadwordVec[1]
  previewVec <- HeadwordVec[1:3]
}
      lapply(previewVec, function(x) {print(x);AssembleEntry(DictData, RuleList, x, projectType="shinyApp")})
      #MakeMediaRepoForShiny(RuleList, HeadwordVec[1:3], Cores)
      WriteShinyApp(previewWord, previewVec, Cores)

      #WriteShinyApp(previewWord)
    print(previewVec)
      setwd("./MyDict")
      RunRCommand <- paste0("Rscript -e ","'library(methods); shiny::runApp(",'"app.R", launch.browser = TRUE',")'")
      system(RunRCommand, intern=T)
      setwd("../")

      }else{
        print("no entry-making rule list found. please create some rules using the menus to the left")
      }

    })


    if(file.exists("./previewWord.txt")){
      previewWord <- readtext("./previewWord.txt")$text
            updateTextInput(session, "Word", value=previewWord)
    }

    AvailableData <- GetAvailableData(DictData)


    updateSelectInput(session, "DataSource",
                      choices=AvailableData, selected=AvailableData[1])

    AvailableVars <- c()


    observeEvent(c(input$DataSource, input$Word), {


      if(!is.null(input$Word) && nchar(input$Word)>0 && nchar(input$DataSource)>0){

        # Word <- str_replace_all(input$Word, "[^[[:lower:]][[:upper:]]\\d\\s-{}]"," ")
        # Word <-  gsub("^\\s+|\\s+$","", Word)

        print(paste("input$Word",input$Word))

        updateCheckboxGroupInput(session, "DataVar",
                                 choices=GetAvailableVars(input$DataSource, DictData, input$Word))

        if(length(intersect(input$Word, HeadwordVec))==1){
          write(as.character(input$Word),"./previewWord.txt")
        }

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


    })

    observeEvent(input$delete,{
      Word <- str_replace_all(input$Word, "[^[[:lower:]][[:upper:]]\\d\\s-{}]"," ")
      Word <-  gsub("^\\s+|\\s+$","", Word)
      if(file.exists("./DictMakingRuleList.rds")){

        RuleList <- readRDS("./DictMakingRuleList.rds")
        RuleList <-  RuleList[names(RuleList)!=input$modify]
        saveRDS(RuleList,"./DictMakingRuleList.rds")
        RuleListJson <- jsonlite::toJSON(RuleList, pretty = T)
        write(RuleListJson, "./DictMakingRuleList.json")
        AssembleEntry(DictData, RuleList, Word, projectType="shinyApp")
        # MakeQuartoFile(Word, readRDS(paste0("./MyDict/",dir("./MyDict")[str_detect(dir("./MyDict"), paste0("^Entry_xx",SafeEncodeUTF8(Word)))])),TAB= GetTABS("./Instructions.txt"))
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
            AssembleEntry(DictData, RuleList, Word, projectType="shinyApp")
            # MakeQuartoFile(Word, readRDS(paste0("./MyDict/",dir("./MyDict")[str_detect(dir("./MyDict"), paste0("^Entry_xx",SafeEncodeUTF8(Word)))])),TAB= GetTABS("./Instructions.txt"))
          }else{
            print(paste("the following sections are not accounted for:", paste(setdiff(names(RuleList), NewOrder), collcapse=";")))
          }

        }
        updateCheckboxInput(session, "rearrangeGo",value=F)
      }
    })
    observeEvent(c(input$addTabs,input$addTabsGo),{
      if(input$addTabsGo==T){

        instructions <- gsub("[;\\(]\\s+|\\s+[;\\)])","",as.character(input$addTabs)) # sanitizes spaces in user-inputted variable names

        RuleList <- readRDS("./DictMakingRuleList.rds")
        InstructionsVars <- unlist(str_extract_all(instructions,"(SEARCHBY|TAB|COL)\\(.*?\\)"))%>%
          str_remove_all("(SEARCHBY|TAB|COL)\\(|\\)")%>%
          str_split(";")%>%
          unlist(.)
        if(length(setdiff(InstructionsVars, names(RuleList)))==0){
          write(instructions, "./Instructions.txt")
        }else{
          msg <- print(paste(setdiff(InstructionsVars, names(RuleList)), "is not in your data, please revise your ui instructions"))
          showModal(
            modalDialog(
              title = "variable not found",
              msg,
              easyClose = TRUE,
              footer = NULL
            )
          )
        }
        updateCheckboxInput(session, "addTabsGo",value=F)
      }
    })

    observeEvent(input$doAllEntries,{

      if(file.exists("./DictMakingRuleList.rds")){
        RuleList <- readRDS("./DictMakingRuleList.rds")
        if (Sys.info()[['sysname']]!="Windows"){
     mclapply(HeadwordVec, function(x) AssembleEntry(DictData, RuleList, x, projectType="shinyApp"), mc.cores=Cores)

        }else{
     lapply(HeadwordVec, function(x) AssembleEntry(DictData, RuleList, x, projectType="shinyApp"))

        }

        showModal(
          modalDialog(
            title = "done",
            "all entries are in MyDict/data",
            easyClose = TRUE,
            footer = NULL
          )
        )

      }else{
        print("create entry-making rules first")
      }

    })

    observeEvent(input$renderDict,{

      if(file.exists("./DictMakingRuleList.rds")){
        RuleList <- readRDS("./DictMakingRuleList.rds")
      }else{
        print("create entry-making rules first")
      }

      if(file.exists("./previewWord.txt")){
        previewWord <- readtext("./previewWord.txt")$text
      }else{
        previewWord <- HeadwordVec[1]
      }

      WriteShinyApp(previewWord, HeadwordVec, Cores)

      ExportShinyDict()

      showModal(
        modalDialog(
          title = "done",
          "the MyDict subfolder contains your web-ready shiny dictionary",
          easyClose = TRUE,
          footer = NULL
        )
      )

    })


  })
}
