
GdexSorterUI <- function(id){

  tagList(
    column(width=4,
    downloadBttn(NS(id,"DownloadGdexTemplate"), "Download gdex template", style="bordered", color="primary", size="xs"),
    selectInput(NS(id, "minLen"), "minimum ideal example length", 3:15),
    selectInput(NS(id, "maxLen"), "maximum ideal example length", 5:30),
    selectInput(NS(id, "LongerPen"), "penalty for each token exceeding maximum ideal length", seq(from=0, to= 10, by=0.5)),
    selectInput(NS(id, "ShorterPen"), "penalty for each token short of minimum ideal length", seq(from=5, to= 20, by=5)),
    selectInput(NS(id, "collocateBoost"), "bonus sentences featuring a collocate (requires 'CollocateCandidates[...].csv' in ./data/Outputs)", 0:30),

    textInput(NS(id,"GdexRules"), "path to csv with your GDEX rules - see documentation" ),

    selectInput(NS(id, "SampleSize"), "number of examples to be retrieved", c(5,10,20,50,100)),
    selectInput(NS(id, "MetaVar"), "sample by subcorpus", choices = NULL),
    selectInput(NS(id, "Var"), "sample by corpus variable", choices = NULL),
    selectInput(NS(id, "wordFormVar"), "name of corpus variable containing word forms", choices = NULL),
    selectInput(NS(id, "MinScore"), "minimum score for examples to be sampled", choices = c("don't apply",0, 10, 20, 50, 100, 200)),
    checkboxGroupInput(NS(id,"AdditionalVars"), "add corpus variables for filtering", choices=NULL),

    textInput(
      NS(id, "SampleWords"),
      "input one or more words to test on (space-separated)"
    ),
    actionButton( NS(id, "test"), label = "test on sample words only"),
    actionButton( NS(id, "process"), label = "process all headwords"),
    tags$br(),
    tags$hr(),

    checkboxGroupInput(NS(id,"combineMeta"), "metadata variables to add", choices=NULL),
    checkboxInput(NS(id,"combine"),"add", value = F),
    tags$hr(),
    ),

  column(width=8,
         tableOutput(NS(id,"sentLenStats")),
         DT::dataTableOutput(NS(id, "SampledSents"))
  ),




  htmlOutput(NS(id,"done"))
  )
}


GdexSorterServer <- function(id, DictData, sIDindexDir="./data/CorpusData/", fileIdentifier="sIDindex_", HeadwordVar, MetadataPath=NULL, Cores){
  moduleServer(id, function(input, output, session){


    output$DownloadGdexTemplate <- downloadHandler(
      filename = function() {
        paste("DDL_GdexRulesTemplate", Sys.time(), ".csv", sep="")
      },

      content = function(file) {
        GdexRulesTemplate <- data.frame(variable="", value="", operation="", penalty_bonus="")
        write.csv(GdexRulesTemplate, file, row.names = F)
      }
    )

    if(length(dir("./data/CorpusDocs"))>1 && length(dir("./data/CorpusData/")[str_detect(dir("./data/CorpusData/"),"sIDindex_")]) > 0){

      SentLenStatsDF <- GetSentLengthStats(sIDindexDir="./data/CorpusData/", fileIdentifier="sIDindex_")
      output$sentLenStats <- renderTable({SentLenStatsDF})

      HeadFreqs <- fread("./data/Outputs/HeadwordFreqs.csv")

     if(file.exists("./data/CorpusMetadata.csv")){

       MetaDF <- fread("./data/CorpusMetadata.csv")

     }else{
       MetaDF <- NULL
     }

     if(!is.null(MetaDF)){
       MetaVarChoices <- c("don't sample",colnames(MetaDF))
     }else{
       MetaVarChoices <- "don't sample"
     }


      CollocatesDF <- GetDFfromFile(Dir="./data/Outputs",filenameString="CollocateCandidates")
      CorpusVars <- colnames(read.fst(paste0("./data/CorpusDocs/", dir("./data/CorpusDocs")[1])))
      updateSelectInput(session,"wordFormVar",
                        choices = CorpusVars)
      updateSelectInput(session,"Var",
                        choices = c("don't sample", CorpusVars))
      updateSelectInput(session,"MetaVar",
                        choices = MetaVarChoices)
      updateCheckboxGroupInput(session, "AdditionalVars", choices = CorpusVars[CorpusVars!=HeadwordVar])

  observeEvent(input$test,{

 if(!is.null(input$GdexRules) &&  TRUE %in% str_detect(input$GdexRules, "\\.csv$")){

  if( file.exists(input$GdexRules)){
  GdexRulesDF <- read.csv(input$GdexRules, stringsAsFactors = F)
  }else{
    print("incorrect filepath for gdex rule. please input path to csv file with your rules")
   }
 }else{
   GdexRulesDF<- NULL
 }

 SampleSize <- as.numeric(input$SampleSize)
 LenRange <- as.numeric(input$minLen:input$maxLen)
 LongerPen <- as.numeric(input$LongerPen)
 ShorterPen <- as.numeric(input$ShorterPen)
 MetaVar <- input$MetaVar
 AdditionalVars <- input$AdditionalVars
 if(MetaVar=="don't sample"){
   MetaVar <- NULL
 }
 Var <- input$Var

 if(Var=="don't sample"){
   Var <- NULL
 }

 MinScore <- input$MinScore
 if(MinScore=="don't apply"){
   MinScore <- NULL
 }else{
   MinScore <- as.numeric(MinScore)
 }

 collocateBoost <- as.numeric(input$collocateBoost)
 wordFormVar <- as.character(input$wordFormVar)
 LemVec <- unlist(str_split(as.character(input$SampleWords)," "))

 SampSents <- lapply(LemVec, function(x)
   SortAndSample(CorpusDocsDir="./data/CorpusDocs", HeadFreqs, HeadwordVar, wordFormVar, x, GdexRulesDF,CollocatesDF,collocateBoost, LenRange, LongerPen, ShorterPen, Var, SampleSize, MetaDF, MetaVar, MinScore, AdditionalVars, Cores)
   )
 names(SampSents) <- LemVec

 SampSentsDF <- rbindlist(SampSents, idcol="lemma")


 output$SampledSents <- DT::renderDataTable({

   if(!is.null(SampSentsDF) && nrow(SampSentsDF)>0){

     ScoredSentsForDisplay <- CreateHorizontalGdexSentDF(SampSentsDF, wordFormVar, AdditionalVars)

     datatable(ScoredSentsForDisplay, escape=F, rownames= FALSE)
   }else{
     showModal(
       modalDialog(
         title = "no data",
         "no sentences matching your parameters",
         easyClose = TRUE,
         footer = NULL
       ))
   }

 })


    })

  observeEvent(input$process,{

    # process all lemmas in headwords list or DictData

    if(!is.null(DictData )){
      if(!is.data.frame(DictData) && !is.null(names(DictData))){
        HeadwordVec <-  intersect(names(DictData), HeadFreqs$lemma)
        if(length(HeadwordVec)==0){
          HeadwordVec <- unique(HeadFreqs$lemma)
        }
      }else if(is.data.frame(DictData)){
        HeadwordVec <-  intersect(DictData[,1], HeadFreqs$lemma)
        if(length(HeadwordVec)==0){
          HeadwordVec <- unique(HeadFreqs$lemma)
        }
      }
    }else{
      HeadwordVec <- unique(HeadFreqs$lemma)
      #HeadwordVec <- HeadwordVec[str_detect(HeadwordVec, "[A-z]")] # attempts to remove noise from uncleaned corpora

    }

    if(!is.null(input$GdexRules) &&  TRUE %in% str_detect(input$GdexRules, "\\.csv$")){

      if( file.exists(input$GdexRules)){
        GdexRulesDF <- read.csv(input$GdexRules, stringsAsFactors = F)
      }else{
        print("incorrect filepath for gdex rule. please input path to csv file with your rules")
      }
    }else{
      GdexRulesDF<- NULL
    }

    AdditionalVars <- input$AdditionalVars
    SampleSize <- as.numeric(input$SampleSize)
    LenRange <- as.numeric(input$minLen:input$maxLen)
    LongerPen <- as.numeric(input$LongerPen)
    ShorterPen <- as.numeric(input$ShorterPen)
    MetaVar <- input$MetaVar
    if(MetaVar=="don't sample"){
      MetaVar <- NULL
    }
    Var <- input$Var
    if(Var=="don't sample"){
      Var <- NULL
    }

    MinScore <- input$MinScore
    if(MinScore=="don't apply"){
      MinScore <- NULL
    }else{
      MinScore <- as.numeric(MinScore)
    }

    collocateBoost <- as.numeric(input$collocateBoost)
    wordFormVar <- as.character(input$wordFormVar)

    SampSents <- mclapply(HeadwordVec, function(x)
      SortAndSample(CorpusDocsDir="./data/CorpusDocs", HeadFreqs, HeadwordVar, wordFormVar, x, GdexRulesDF,CollocatesDF,collocateBoost, LenRange, LongerPen, ShorterPen, Var, SampleSize, MetaDF, MetaVar, MinScore, AdditionalVars, Cores),
      mc.cores=Cores)

    # SampSents <- lapply(HeadwordVec, function(x)
    #   SortAndSample(CorpusDocsDir="./data/CorpusDocs", HeadFreqs, HeadwordVar, wordFormVar, x, GdexRulesDF,CollocatesDF,collocateBoost, LenRange, LongerPen, ShorterPen, Var, SampleSize, MetaDF, MetaVar, MinScore, AdditionalVars, Cores))
    #
    names(SampSents) <- HeadwordVec
    SampSentsDF <- rbindlist(SampSents, idcol="lemma", fill = T)


      if(!is.null(SampSentsDF) && nrow(SampSentsDF)>0){
        ScoredSentsForDisplay <- CreateHorizontalGdexSentDF(SampSentsDF,wordFormVar, AdditionalVars)
        ScoredSentsForDisplay <- ScoredSentsForDisplay[,which(colnames(ScoredSentsForDisplay) %in% c("lemma","ID","Sent", AdditionalVars))] # retains only ID & Sent cols
        fwrite(ScoredSentsForDisplay, "./data/Outputs/ExampleSents.csv")
      }else{
        showModal(
          modalDialog(
            title = "no data",
            "no sentences matching your parameters",
            easyClose = TRUE,
            footer = NULL
          ))
      }



  output$done <- renderText({
    req(SampSentsDF)
    showModal(
      modalDialog(
        title = "done",
        "done!",
        easyClose = TRUE,
        footer = NULL
      )
    )

  })
    })
    }else{
     print("no suitabe corpus data in ./data/CorpusData/ : make sure you convert corpus before using this section")
   }


observe({
  if(file.exists("./data/CorpusMetadata.csv")){

    MetaDF <- fread("./data/CorpusMetadata.csv")

  }else{
    MetaDF <- NULL
  }
  if(!is.null(MetaDF) && file.exists("./data/Outputs/ExampleSents.csv")){

      cols <- colnames(MetaDF)
      if(length(intersect("filename", cols))==1){
        updateCheckboxGroupInput(session, "combineMeta", choices =  cols[cols!="filename"])
      }
    }
  })

observeEvent(c(input$combineMeta,input$combine),{

  if(input$combine==T && !is.null(MetaDF) && !is.null(input$combineMeta) && length(input$combineMeta)>0){
     ex <- read.csv("./data/Outputs/ExampleSents.csv", stringsAsFactors = F)
    ex$filename <- gsub("^\\d+___(.*?)$","\\1" , ex$ID)
    ex <- left_join(ex, MetaDF[, c("filename",input$combineMeta)], by="filename")

    write.csv(ex, "./data/Outputs/ExampleSentsWithMetadata.csv",row.names = F)
    updateCheckboxInput(session,"combine", value=F )
  }

  })

  })
}







