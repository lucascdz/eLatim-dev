
# writes CollocateCandidates.csv in Outputs for editing outside of app

CollocationsUI <- function(id){

  tagList(
    fluidRow(
      # selectInput(
      #   NS(id, "HeadwordVar"),
      #   "headword variable",
      #   choices = NULL,
      #
      # ),


      column(width=4,
             textInput(NS(id,"stopwordsFile"),"path to csv file with stopwords (optional)"),

             selectInput(
               NS(id, "window"),
               "kwic window",
               choices = c(seq(from=1, to=10, by=1)),
               selected = 3
             ),
             selectInput(
               NS(id, "minFreq"),
               "minimum frequency of collocate candidates",
               choices = c(10, 50, 100, 500,1000,5000,10000),
               selected = 100
             ),
             selectInput(
               NS(id, "LogLik"),
               "Log Likelihood Threshold",
               choices = c(seq(from=5, to=500, by=5)),
               selected = 10
             ),
             selectInput(
               NS(id, "LR"),
               "Log Ratio Threshold",
               choices = c(seq(from=0, to=10, by=0.5)),
               selected = 1
             ),
             selectInput(
               NS(id, "MI"),
               "Mutual Information Threshold",
               choices = c(seq(from=0, to=10, by=1)),
               selected = 0
             ),
             selectInput(
               NS(id, "LogDice"),
               "Log Dice Threshold",
               choices = c(seq(from=0, to=14, by=1)),
               selected = 0
             ),
             selectInput(
               NS(id, "sortBy"),
               "sort by",
               choices = c("LogL","LogR","MI","LogDice"),
               selected = "LogLikelihood"
             ),
             selectInput(
               NS(id, "maxCollo"),
               "maximum number of collocations per headword",
               choices = c(seq(from=2, to=100, by=1)),
               selected = 20
             ),
             textInput(
               NS(id, "SampleWords"),
               "input a few sample words to test on (space-separated)"
             ),
             actionButton( NS(id, "test"), label = "test on sample words only"),
             tags$br(),
             tags$hr(),
             actionButton( NS(id, "process"), label = "process all headwords"),
             tags$br(),
             tags$br(),
             tags$br(),
      ),
      column(width=8,
             DT::dataTableOutput(NS(id, "SampleWordsRes"))
        )
    ),

    htmlOutput(NS(id,"done"))

  )


}


CollocationsServer <- function(id, DictData, HeadwordVar, Cores){
  moduleServer(id, function(input, output, session) {


if(file.exists("./data/Outputs/HeadwordFreqs.csv") && length(dir("./data/CorpusDocs"))>0){

  # updateSelectInput(session, "HeadwordVar",
  #                   choices= colnames(read.fst(paste0("./data/CorpusDocs/",dir("./data/CorpusDocs")[1]))) )
  # # find HeadwordVar (in case Collocation tab is used in new session without the extractionRules csv)
  # # HeadwordVar will be the column in a fst corpus doc whose value are all included in the lemma col of the freqs dataset:
  # aCorpusDoc <- read.fst(paste0("./data/CorpusDocs/",dir("./data/CorpusDocs")[1]))
  # lemmaColCandidates <- unlist(lapply(colnames(aCorpusDoc), function(x) length(intersect(aCorpusDoc[[x]],HeadFreqs$lemma ))))
  # HeadwordVar <- colnames(aCorpusDoc)[which.max(lemmaColCandidates)]


observeEvent(input$test,{
  HeadFreqs <- fread("./data/Outputs/HeadwordFreqs.csv")

  if(!is.null(input$SampleWords) &&  nchar(input$SampleWords)>0){

  HeadwordVec <-  input$SampleWords
  HeadwordVec <- unlist(str_split(HeadwordVec,",\\s?|\\s"))
  HeadwordVec <- gsub(" ","", HeadwordVec)
  HeadwordVec[is.na(HeadwordVec)] <- ""
  #HeadwordVec <- HeadwordVec[HeadwordVec %in% HeadFreqs[[HeadwordVar]]]
  HeadwordVec <- HeadwordVec[HeadwordVec %in% HeadFreqs$lemma]

  if(!is.null(HeadwordVec) && length(HeadwordVec)>0){

    window <- input$window
    MinFreq <- input$minFreq
    minLL <- input$LogLik
    minLR <- input$LR
    minMI <- input$MI
    minLogDice <- input$LogDice

    
    stopwordsFile <- input$stopwordsFile
  if(!is.null(stopwordsFile) &&  TRUE %in% str_detect(as.character(stopwordsFile), "\\.csv$")){
    Stopwords <- read.csv(stopwordsFile, stringsAsFactors = F)
    Stopwords <- Stopwords[,1]
 
  }else{
    Stopwords <- ""
  }
    SortBy <- input$sortBy
    
    if(SortBy=="LogR"){
    SortBy <- "LR"
    }else if(SortBy=="LogL"){
      SortBy <- "LogLikCorpusVSRefcorpus"
    }
    
    MaxKW <- input$maxCollo
    
    
  output$SampleWordsRes <- DT::renderDataTable ({
print("Collocations line 149 ")
    print(paste("window",window))
    print(paste("MinFreq",MinFreq))
    print(paste("minLL",minLL))
    print(paste("minLR",minLR))
    print(paste("minLogDice",minLogDice))
    print(paste("minMI",minMI))
    print(paste("Stopwords 1",Stopwords[1]))
    KeyWDF <- GetColloForAllLemmaAtOnce(HeadwordVec, HeadFreqs, window, HeadwordVar, Stopwords, MinFreq, minLL, minLR, minMI,minLogDice,SortBy,MaxKW,Cores)
print(head(KeyWDF))
    if(!is.null(KeyWDF) && nrow(KeyWDF)>0){
    datatable(KeyWDF, escape=F, rownames= FALSE)
    }else{
      showModal(
        modalDialog(
          title = "no data",
          "no collocations matching your parameters",
          easyClose = TRUE,
          footer = NULL
        ))
    }
  })

  }else{
    showModal(
      modalDialog(
        title = "out of vocabulary",
        "the inputted headwords are not your corpus",
        easyClose = TRUE,
        footer = NULL
      ))
  }
  }else{
    showModal(
      modalDialog(
        title = "no words to test on",
        "input a few words for testing",
        easyClose = TRUE,
        footer = NULL
      ))
  }

})



observeEvent(input$process,{
  HeadFreqs <- fread("./data/Outputs/HeadwordFreqs.csv")


 # process all headwords together and write a single csv for all of them (alternative solution after this)
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
    HeadwordVec <-HeadwordVec[str_detect(HeadwordVec, "[A-z]")] # attempts to remove noise from uncleaned corpora
  }

  print(paste("length(HeadwordVec)",length(HeadwordVec)))

  window <- input$window
  MinFreq <- input$minFreq
  minLL <- input$LogLik
  minLR <- input$LR
  minMI <- input$MI
  minLogDice <- input$LogDice
  stopwordsFile <- input$stopwordsFile
  if(!is.null(stopwordsFile) &&  TRUE %in% str_detect(as.character(stopwordsFile), "\\.csv$")){
    Stopwords <- read.csv(stopwordsFile, stringsAsFactors = F)
    Stopwords <- Stopwords[,1]
  }else{
    Stopwords <- ""
  }
  SortBy <- input$sortBy
  MaxKW <- input$maxCollo

  #  KeyWDF <- GetColloForAllLemmaAtOnce(HeadwordVec, HeadFreqs, window, HeadwordVar, Stopwords, MinFreq, minLL, minLR, minMI, minLogDice, SortBy, MaxKW, Cores)
  # if(!is.null(KeyWDF) && length(KeyWDF)>0){
     parameters <- paste0("MinFreq",MinFreq,"minLL",minLL,"minLR",minLR,"minMI",minMI,"minLogDice",minLogDice)
  # fwrite(KeyWDF, paste0("./data/Outputs/CollocateCandidates_",parameters ,".csv"))

  # alternative solution, process headwords one by one and write one file per headword
  # store files in new subfolder 'collocates'
  # GetColloByLemma also writes the files, so no need for the parameters/fwrite lines with it
     if (Sys.info()[['sysname']]!="Windows"){
     KeyWDFs <- mclapply(HeadwordVec, function(x) GetColloByLemma(x, HeadFreqs, window, HeadwordVar, Stopwords, MinFreq, minLL, minLR, minMI,minLogDice,SortBy,MaxKW,Cores),mc.cores=Cores)
     }else{
       KeyWDFs <- lapply(HeadwordVec, function(x) GetColloByLemma(x, HeadFreqs, window, HeadwordVar, Stopwords, MinFreq, minLL, minLR, minMI,minLogDice,SortBy,MaxKW,Cores))
     }
  KeyWDF <- do.call(rbind, KeyWDFs)
  fwrite(KeyWDF, paste0("./data/Outputs/CollocateCandidates_",parameters ,".csv"))

  if(file.exists("./data/Outputs/Collocations")){
  system("rm -r ./data/Outputs/Collocations")
  }

  # }else{
  #   showModal(
  #     modalDialog(
  #       title = "no data",
  #       "no collocations matching your parameters",
  #       easyClose = TRUE,
  #       footer = NULL
  #     ))
  # }

  output$done <- renderText({
    req(KeyWDF)
    showModal(
      modalDialog(
        title = "done",
        "done! a table with the collocate candidates is ready for you to edit in in data > Outputs",
        easyClose = TRUE,
        footer = NULL
      )
    )

  })

})
    }else{
      showModal(
        modalDialog(
          title = "no data",
          " make sure HeadwordFreqs.csv is in data > Outputs",
          easyClose = TRUE,
          footer = NULL
        )
      )
    }


  })
}
