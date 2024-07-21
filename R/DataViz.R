
#  IMPORTANT currently choice of lemma to test on relies on fact that lemma/headword col is the FIRST COL in DATASET

# for each type of chart (barchart, wordcloud, zipfcurve ... add others as per participants' requests)
# select csv dataset (HeadwordFreq; CollocatateCandidates, SubcorporaHeadwordFreq ... allow to uplaod costum dataset, e.g. synonyms)
# select type of chart
# select variable to visualize
# preview chart
# save chart with name of lemma, name of vars, typeofchart
# write also a file with settings for the chart?

# add option to also visualize dictdata (e.g. chelo wants to visualise domain tags)

# slicker option is to use renderUI and render in the same spot whichever type of graph is selcte
# but you can also assign diffent portion of the screen to different graph-types..

DataVizUI <- function(id){

  tagList(

    fluidRow(
      column(width = 4,
             selectInput(NS(id,"VarTwo"),
                         "select variable to visualise by",
                         choices=NULL),
             selectInput(NS(id,"VarOne"),
                         "select main variable to visualise",
                         choices=NULL),
             textInput(NS(id,"testWord"), "one word to test on"),
             #actionButton(NS(id,"go"),"test"),
             checkboxInput(NS(id,"go"),"test", value=F),

      ),
      column(width=8,
             uiOutput(NS(id,"viz"))
      )
    )

  )
}


DataVizServer <- function(id, DictData, DataFile, chartType, HeadwordVar, Cores){
  moduleServer(id, function(input, output, session) {

    if(!file.exists("./data/Plots")){
      system("mkdir ./data/Plots")
    }
    if(!file.exists("./data/AltText")){
      system("mkdir ./data/AltText")
    }

observe({

  if(!is.null(DataFile) && DataFile!=""){
    if(DataFile=="CorpusData"){
      Data <-  read.fst(paste0("./data/CorpusDocs/", dir("./data/CorpusDocs")[1]))
    }else{
      if(str_detect(as.character(DataFile), "csv$")){
        Data <- read.csv(paste0("./data/Outputs/", as.character(DataFile)), stringsAsFactors = F)
      }
    }
  }else{
    Data <- NULL
  }


  # restricting ChoicesVarTwo by chartType
  if(chartType %in% c("wordcloud","logcurve")){
    ChoicesVarTwo <-  c("",colnames(dplyr::select_if(Data, is.numeric)))
  }else if(chartType=="barchart" && TRUE %in% str_detect(colnames(Data), "_DP$")) {
    vars <- gsub("^(.*?)_DP$","\\1", colnames(Data)[str_detect(colnames(Data),"_DP")] )
    ChoicesVarTwo <- paste(vars, "Frequencies", sep="_")
  }else if(chartType==""){
    ChoicesVarTwo <- NULL
  }else{ # network data
    ChoicesVarTwo <- colnames(Data)
  }

  #print("DataViz 80")
  # restricting ChoicesVarOne by chartType
  if(chartType %in% c("wordcloud","logcurve")){
    ChoicesVarOne <-  c("",colnames(dplyr::select_if(Data, is.character)))
  }else if(chartType=="barchart" && TRUE %in% str_detect(ChoicesVarTwo, "_DP$")) {
    ChoicesVarOne <- "lemma"
  }else if(chartType==""){
      ChoicesVarOne <- NULL
  }else{ # network data
      ChoicesVarOne <- colnames(Data)
  }


# updateSelectInput(session, "VarOne",
#                   choices=ChoicesVarOne
# )
#
# observeEvent(input$VarOne, {
#   updateSelectInput(session, "VarTwo",
#                     choices=ChoicesVarTwo
#   )
# })

  updateSelectInput(session, "VarTwo",
                    choices=ChoicesVarTwo
  )

  observeEvent(input$VarTwo, {
    updateSelectInput(session, "VarOne",
                      choices=ChoicesVarOne
    )
  })



  if(input$go==T && input$VarOne!="" && input$VarTwo!=""){

  # get vector of headword to create plots for when 'plot all' is clicked
  HeadFreqs <- fread("./data/Outputs/HeadwordFreqs.csv")
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


# create plots based on inputs

 if(!is.null(Data) && !is.null(input$testWord) && input$testWord!="" && input$VarTwo!="" && input$VarOne!=""){

 if(length(intersect("lemma",colnames(Data)))>0 && !input$testWord %in% Data$lemma){

   print(paste(input$testWord, " is not the corpus list; input a headword that is in your corpus"))
   }else{

    if(identical(colnames(Data), colnames(read.fst(paste0("./data/CorpusDocs/", dir("./data/CorpusDocs")[1]))) )){

       DF <- GetSENTS(HeadwordVar = HeadwordVar, LEM=as.character(input$testWord), Cores=Cores )
       DF <- DF[DF[[HeadwordVar]] == as.character(input$testWord),]
     }else{

     if(input$VarTwo %in% colnames(Data)){

       DF <- Data[Data$lemma==input$testWord, colnames(Data) %in% c(as.character(input$VarOne), input$VarTwo)]
     }else if(str_detect(input$VarTwo, "_Frequencies")) {

       Metavar <- gsub("^(.*?)_Frequencies","\\1", input$VarTwo)
       DF <- Data[Data$lemma==input$testWord,]
       DF <- DF[,colnames(DF)[str_detect(colnames(DF), "Freq")]]
       if(file.exists('./data/CorpusMetadata.csv')){

         Meta <- read.csv('./data/CorpusMetadata.csv')
         DF <- DF[,colnames(DF)[str_detect(colnames(DF), paste(Meta[[Metavar]], collapse = "|"))]]
       }
     }else{
       DF <- Data
     }
     }


if(chartType=="wordcloud"){

     output$viz <- renderUI({
       ClickableWordcloudUI(NS(id,"wordCloud"))
     })
    if(!is.null(DF) && is.data.frame(DF) && nrow(DF)>1 && length(intersect(colnames(DF),as.character(input$VarTwo)))>0 && is.numeric(DF[[as.character(input$VarTwo)]])){
      vizdata <-  Data[, colnames(Data) %in% c("lemma", as.character(input$VarOne), as.character(input$VarTwo))]

         ClickableWordcloudServer("wordCloud",HeadwordVec, DF, vizdata, Cores)

    }


}else if(chartType=="barchart"){

  output$viz <- renderUI({
    BarchartEssentialUI(NS(id,"barChart"))
  })

  FullData <- Data

  if(str_detect(input$VarTwo, "_Frequencies")) {

    Metavar <- gsub("^(.*?)_Frequencies","\\1", input$VarTwo)
    FullData <- FullData[,c("lemma",colnames(FullData)[str_detect(colnames(FullData), "Freq")])]
    if(file.exists('./data/CorpusMetadata.csv')){
      Meta <- read.csv('./data/CorpusMetadata.csv')
      FullData <- FullData[,c("lemma",colnames(FullData)[str_detect(colnames(FullData), paste(Meta[[Metavar]], collapse = "|"))])]
    }
  }

  BarchartEssentialServer(id="barChart", HeadwordVec, HeadwordVar=HeadwordVar, Dataset= DF, FullData= FullData , xInput= as.character(input$VarOne), fillInput= as.character(input$VarTwo),LEMMA=input$testWord, Cores=Cores)

}else if(chartType=="logcurve"){
  output$viz <- renderUI({
    logcurveUI(NS(id,"logCurve"))
  })

  FreqsDF <- Data
  if( identical( (c(as.character(input$VarTwo), "lemma") %in% colnames(Data)), c(TRUE,TRUE) )){
  freqCol <- as.character(input$VarTwo)
  if(length(unique(FreqsDF$lemma))==length(FreqsDF$lemma)){

  FreqsDF$lemma <- factor(FreqsDF$lemma, levels = FreqsDF$lemma[order(-as.numeric(FreqsDF[[freqCol]]))])

  MainPlot <- ggplot(FreqsDF,aes(lemma,log(FreqsDF[[freqCol]])))+geom_point(color="gray",show.legend = FALSE) + theme_void()

  logcurveServer("logCurve",HeadwordVec, FreqsDF,input$testWord, freqCol, MainPlot, Cores)


  }else{
    print("change dataset: lemma column must have unique values")
    # showModal(
    #   modalDialog(
    #     title = "unsuitable dataset",
    #     "change dataset: lemma column must have unique values",
    #     easyClose = TRUE,
    #     footer = NULL
    #   )
    # )
  }
  }else{
    print(paste("change to a dataset with columns 'lemma' and '", as.character(input$VarTwo),"'.", collapse = ""))
    # showModal(
    #   modalDialog(
    #     title = "unsuitable dataset",
    #     paste0("change to a Frequencies dataset with columns 'lemma' and '", as.character(input$VarTwo),"'."),
    #     easyClose = TRUE,
    #     footer = NULL
    #   )
    # )
  }

}
    }
 }

  updateCheckboxInput(session,"go", value=F)

  }
})

  })
}
