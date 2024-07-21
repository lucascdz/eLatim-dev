MakeSharableDataUI <- function(id){
  tagList(

    sidebarLayout(
      sidebarPanel(width=2,
                   actionButton(NS(id,"doAll"),"standardize all entries")
      ),

      mainPanel(
        fluidRow(
          column(width=4,
                 selectInput(NS(id,"elementName"),"element name",choices=NULL),
          ),
          column(width=4,
                 selectInput(NS(id,"standardName"),"standardized name",choices=NULL)
          ),
          column(width=4,
                 textInput(NS(id,"lang"),"language",value="", placeholder="2-letter ISO-639 code")
          ),
          actionButton(NS(id,"standardize"),"apply",value=F),
          actionButton(NS(id,"deleteRow"),"delete selected row"),
          dataTableOutput(NS(id,"DF")
        )
      )
    )
   )
  )
}


MakeSharableDataServer <- function(id, Cores){
  moduleServer(id, function(input, output, session) {
if(file.exists("./DictMakingRuleList.rds")){
    Rules <- readRDS("./DictMakingRuleList.rds")
    standardName <- c('headword','PoS' ,'equivalent','definition','example','equivalent_example','multi-word expression', "variant")
    lexicogName <- c('ontolex:LexicalEntry','lexinfo:partOfSpeech' ,'vartrans:Translation', 'ontolex:LexicalConcept', 'lexicog:UsageExample','lexicog:UsageExample',"ontolex:MultiwordExpression","ontolex:writtenRep" )
    lexicogNamesDF <- data.frame(standardName, lexicogName, stringsAsFactors = F)
    write.csv(lexicogNamesDF, "./lexicogNamesDF.csv",row.names = F)


    updateSelectInput(session,"elementName", choices= names(Rules) )

    updateSelectInput(session,"standardName", choices=c(lexicogNamesDF$standardName, "other") )


    observeEvent(input$standardize, {
      if(file.exists("./standardizedElementNames.csv")){
        DF <- read.csv("./standardizedElementNames.csv", stringsAsFactors = F)
        DF <- rbind(DF, data.frame(elementName=input$elementName, standardName=input$standardName, lang=input$lang))
        DF <- unique(DF)
      }else{
        DF <- data.frame(elementName=input$elementName, standardName=input$standardName, lang=input$lang)
      }
      write.csv(DF, "./standardizedElementNames.csv",row.names = F)

      DF <- left_join(DF, lexicogNamesDF, by="standardName")
      output$DF <- renderDataTable({
        DF
      })
    })

observeEvent(input$deleteRow,{
  if(file.exists("./standardizedElementNames.csv") && !is.null(input$DF_rows_selected)){
    DF <- read.csv("./standardizedElementNames.csv", stringsAsFactors = F)
    DF <- DF[- as.numeric(input$DF_rows_selected), ]
    write.csv(DF, "./standardizedElementNames.csv",row.names = F)
    output$DF <- renderDataTable({
      DF
    })
  }
})


    observeEvent(input$doAll, {
      if(file.exists("./standardizedElementNames.csv")){
        DF <- read.csv("./standardizedElementNames.csv", stringsAsFactors = F)
        DF <- left_join(DF, lexicogNamesDF, by="standardName")
        DF[is.na(DF)]<-""
        DF$lexicogName[DF$lexicogName==""] <- paste("thisResource",DF$elementName[DF$lexicogName==""],sep=":")
        creds <- GetDictCreds()
        context <- CreateContextElement(DF$lexicogName, NameSpacesDF)

        if(file.exists("./MyDict/_quarto.yml")){
          EntriesDir <- "./MyDict/"
        }else{
          EntriesDir <- "./MyDict/data"
        }

        if(!file.exists("./SharableDictData")){
          system("mkdir ./SharableDictData")
        }

        if(length(EntriesDir)){
        # create json-ld entries
          if (Sys.info()[['sysname']]!="Windows"){
             mclapply(dir(EntriesDir)[str_detect(dir(EntriesDir), "^Entry_..*?.rds$")], function(x) MakeSharableEntry(readRDS(paste0(EntriesDir,"/",x)), DF, creds,context), mc.cores=Cores )
          }else{
            lapply(dir(EntriesDir)[str_detect(dir(EntriesDir), "^Entry_..*?.rds$")], function(x) MakeSharableEntry(readRDS(paste0(EntriesDir,"/",x)), DF, creds,context))
          }
        # copy over necessary media files
        MoveFilesToSharebleDict(creds, "./SharableDictData")
        showModal(
          modalDialog(
            title = "done",
            "done! your share-ready files are in the SharableDictData directory",
            easyClose = TRUE,
            footer = NULL
          )
        )
        }else{
        showModal(
          modalDialog(
            title = "warning",
            "create entries in the BuildDictioanry tab first",
            easyClose = TRUE,
            footer = NULL
           )
          )
        }
      }else{
        showModal(
          modalDialog(
            title = "warning",
            "create standardised names first",
            easyClose = TRUE,
            footer = NULL
          )
        )
      }
    })
    }
  })
}
