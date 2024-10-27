

myApp <- function(...) {

  options(shiny.maxRequestSize=50*1024^2) # default is 5MB per file

  addResourcePath("tmpuser", getwd())

  ui <- navbarPage(
    id="lexicographR",
    title="lexicographR: digital dictionary builder",
    windowTitle="lexicographR",
    collapsible = TRUE,
    tabPanel("Home", fluid = TRUE,
             fluidRow(
             column(width=6,
             htmltools::tags$iframe(src = "tmpuser/Home.html", width = '100%',  height = 1000,  style = "border:none;")
             ),
             column(width=6,
              htmltools::tags$iframe(src = "tmpuser/Flowchart.html", width = '100%',  height = 1000,  style = "border:none;")
                    
             )
             )
    ),
    tabPanel("ConvertDictionaryData", fluid = TRUE,
             div(HTML("if your data is in .xlxs or .numbers please first export it to .csv using your preferred spreadsheet editor" )),
             div(HTML("spaces in filenames are not accepted. zipped foders with dictionary data are accepted; all zipped files must have the same structure and file extension")),
             tags$br(),
             LoadDictDataUI(id="LoadDictData",
                            Label="path to your main dictionary data"),

             checkboxInput("converterGo","go", value= FALSE),
             tags$hr(),
             sidebarLayout(
               sidebarPanel(width=4,
                            TabularDataConversionInterfaceUI("HeadSelection")
               ),
               mainPanel(
                 PreviewContentUI(id="FilePreview")
               )
             )
    ),

    tabPanel("ExtractCorpusData", fluid = TRUE,

             div(HTML("To process your corpus and derive frequency and dispersion from it:
              <br />
              <br />
              <ol>
              <li>read the user guide</li>
              <li>download the template for corpus processing instructions</li>
              <li>fill it on your machine using any spreadsheet or text editor, fill one row per corpus directory, starting from the second row, the first row contains instructions on how to fill the fields and it will not be read by the system</li>
              <li>re-upload it here as a csv file. </li>
              <li>optional: upload your rds dictionary data or a csv with a list of headwords you want to process</li>
              <li>review your data extractions rule and, if all seems goo, click 'go' </li>
              <li>if you have a suitable metadata file, once the corpus has been processed, fill the metadata section at the bottom to include subcorpora frequency and dispersion information</li>
              </ol>
              <br />
              we recommend using UTF-8 encoding in your spreadsheet and NOT using spaces or special characters in filenames
              <br /> IMPORTANT: if using metadata for freqs and dispersion, the same metadata must be available and used for all corpora (see instruction in template & documentation).
                   ")),
             tags$br(),
             tags$hr(),
             column(width = 6,

                    downloadBttn("DownloadTemplate", "Download template", style="bordered", color="primary", size="xs"),

                    fileInput("UploadInstructions", "upload the edited csv template",
                              accept = c(
                                "csv",
                                "comma-separated-values",
                                ".csv")
                    )

             ),
             column(width=6,
                    fileInput("UploadDict", "upload your .rds/.json dictionary data or csv with headwords to process",
                              accept = c(
                                "rds", "csv","json"
                              )
                    )),
             tags$br(),
             #tags$hr(),

             column(width = 12,
                    # div(HTML("<font size=+1>review your corpus-data extraction rule:</font>")),
                    DT::dataTableOutput("InstructionsDF"),
                    actionButton("go", label = "go"),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$hr(),
             ),

             #div(HTML("<font size=+1> optional: add metadata information for subcorpora identification </font>")),

             htmlOutput("done"),
             textInput("MetaPath","path to edited corpus metadata file in csv"),
             textInput("SubcorpusVar","corpus metadata file's variable to be used for subcorpora calculations"),

             actionButton("SubcorporaCalc", label = "use metadata"),
             htmlOutput("doneSubcorpora")

    ),
    tabPanel("Collocations", fluid = TRUE,

             CollocationsUI("collocations")

    ),
    tabPanel("Examples", fluid = TRUE,
             GdexSorterUI("gdexSampler")

    ),
    tabPanel("Visualizations",
             mainPanel(width = 12,
                       tabsetPanel(id="inDataViz",
                                   tabPanel("CorpusData", fluid = TRUE,
                                            selectInput("VizDataset",
                                                        "choose dataset",
                                                        choices=NULL),
                                            selectInput("chartType",
                                                        "choose chart",
                                                        choices=c("wordcloud","barchart","logcurve","network")),
                                            DataVizUI("dataviz")
                                   ),
                                   tabPanel("DictionaryData", fluid = TRUE,
                                            selectInput("NetworkType",
                                                        "choose chart type",
                                                        choices=c("tree","diagonalNetwork","radialNetwork")),
                                            DictVizUI("dictviz"),

                                   )
                       )
             )
    ),

    tabPanel("BuildDictionary", fluid = TRUE,

             QuartoEditorUI("editor")
    ),
    tabPanel("ShareData", fluid = TRUE,
             MakeSharableDataUI("share"),

             ),
    tabPanel("Documentation", fluid = TRUE,
             
             #renderMarkdown("./docu/CorpusDataExtraction_UserGuide.Rmd")
             htmltools::tags$iframe(src = "tmpuser/UserGuide.html", width = '100%',  height = 1000,  style = "border:none;")
    ),

  )





  server <- function(input, output, session) {


    # output$flowchart <- renderImage({
    #                     list(src = "tmpuser/flowchartv2.png",
    #                     contentType = 'image/png',
    #                     width = 400,
    #                     height = 300,
    #                     alt = "chart illustrating lexicographR workflow. see user guide for text description of workflow")
    #                     deleteFile=FALSE
    # })
    
    
    if(!file.exists("./data")){
      if (Sys.info()[['sysname']]!="Windows"){

        system("mkdir ./data")
      }else{
        shell("mkdir .\\data")
      }
    }


    observeEvent(input$converterGo , {

      if(input$converterGo==TRUE){

        ConverterDictData <- reactive(LoadDictDataServer(id="LoadDictData"))

        TabularDataConversionInterfaceServer("HeadSelection", ConverterDictData()$Ext , ConverterDictData()$Contents )

        PreviewContentServer(id="FilePreview", ConverterDictData()$Contents)
      }
    })

    output$DownloadTemplate <- downloadHandler(
      filename = function() {
        paste("DDL_CorpusProcessingTemplate", Sys.time(), ".csv", sep="")
      },


      content = function(file) {
        Template <- createTemplate() # function def in SetUp.R
        write.csv(Template, file, row.names = F)
      }
    )



    Uploaded <- reactive({
      if(!is.null(input$UploadInstructions) && TRUE %in% str_detect(as.character(input$UploadInstructions), "\\.csv$") ){
        read.csv(input$UploadInstructions$datapath, header = T, stringsAsFactors = F)
      }else{
        NULL
      }
    })

    observeEvent(input$UploadInstructions,{
      Uploaded <- Uploaded()
      if (!is.null(Uploaded)){
        write.csv(Uploaded, "./data/dataExtractionInstructions.csv",row.names = F )
        MetadataPath <- Uploaded$MetadataPath[2]
        if(!is.null(MetadataPath) && file.exists(MetadataPath)){
          Meta <- fread(MetadataPath)
          fwrite(Meta, "./data/CorpusMetadata.csv")
        }

        output$InstructionsDF <- DT::renderDataTable(
          data.table(Uploaded[-1,])
        )
      }
    })

    observeEvent(input$go,{

      Uploaded <- Uploaded()

      for (i in 2:nrow(Uploaded)){

        CorpusName <- Uploaded$CorpusName[i]
        CorpusDir <- Uploaded$CorpusDir[i]
        CorpusDir <- gsub('"','',CorpusDir)
        CorpusDir <- gsub("'","",CorpusDir)



        HeadwordVar <- Uploaded$HeadwordVar[i]
        HeadwordVar <- gsub("^\\s|\\s$","",HeadwordVar)
        if(str_detect(HeadwordVar, "\\s")){
          HeadwordVar <- unlist(str_split(HeadwordVar,"\\s"))
        }

        columnNames <- Uploaded$columnNames[i]
        if(!is.null(columnNames) && length(columnNames)>0 && TRUE %in% str_detect(columnNames,"\\s")){
          columnNames <- gsub("^\\s+|\\s+$","", columnNames)
          columnNames <- gsub("\\s+"," ", columnNames)
          columnNames <- gsub('"',"", columnNames)
          columnNames <- gsub("'","", columnNames)
          columnNames <- unlist(str_split(columnNames,"\\s"))
        }

        SentEndMarker <- Uploaded$SentEndMarker[i]
        if(SentEndMarker==""){
          SentEndMarker <- NULL
        }
        if(!is.null(SentEndMarker)  && !str_detect(SentEndMarker,"<|>") &&  TRUE %in% str_detect(SentEndMarker,"\\s")){
          SentEndMarker <- paste0("([.?!", gsub("\\s+","", SentEndMarker),"])" )
        }

        ColWithSentMarker <- Uploaded$ColWithSentMarker[i]
        if(ColWithSentMarker==""){
          ColWithSentMarker <- NULL
        }
        TagsVector <- Uploaded$TagsVector[i]

        if(!is.null(TagsVector) &&( length(TagsVector)==0 || TagsVector=="" )){
          TagsVector <- NULL
        }

        if(!is.null(TagsVector) && length(TagsVector)>0 && TRUE %in% str_detect(TagsVector," ")){
          TagsVector <- gsub("^\\s+|\\s+$","", TagsVector)
          TagsVector <- gsub("\\s+"," ", TagsVector)
          TagsVector <- gsub('"',"", TagsVector)
          TagsVector <- gsub("'","", TagsVector)
          TagsVector <- unlist(str_split(TagsVector,"\\s"))
        }


        SentenceBoundaryMarker <- Uploaded$SentenceBoundaryMarker[i]
        SentenceIDtag <- Uploaded$SentenceIDtag[i]
        Language <- Uploaded$Language[i]
        if(SentenceBoundaryMarker==""){
          SentenceBoundaryMarker <- NULL
        }
        if(SentenceIDtag==""){
          SentenceIDtag <- NULL
        }
        if(Language==""){
          Language <- NULL
        }
        Cores <<- Uploaded$Cores[i]
        if(Cores==""|is.na(Cores)){
          Cores <<- 2
        }

        MetadataTag <- Uploaded$MetadataTag[i]

        if(!is.null(MetadataTag) && ( length(MetadataTag)==0 || MetadataTag=="")){
          MetadataTag <- NULL
        }

        ColWithMetadataTag <- Uploaded$ColWithMetadataTag[i]
        if(!is.null(ColWithMetadataTag) && ColWithMetadataTag==""){
          ColWithMetadataTag <- NULL
        }


        preprocessedTargetDir <- Uploaded$preprocessingTargetDir[i]

        if(!is.null(preprocessedTargetDir) && ( length(preprocessedTargetDir)==0 || preprocessedTargetDir=="")){
          preprocessedTargetDir <- NULL
        }


        source("./R/SetUp.R")
        source("./R/FunctionToCreateCorpusObjects.R")


        # convert corpus document and create text freq tables


        if(!file.exists("./data/CorpusData")){
          if (Sys.info()[['sysname']]!="Windows"){

            system("mkdir ./data/CorpusData")
          }else{
            shell("mkdir .\\data\\CorpusData")
          }
        }
        if(!file.exists("./data/CorpusDocs")){
          if (Sys.info()[['sysname']]!="Windows"){

            system("mkdir ./data/CorpusDocs")
          }else{
            shell("mkdir .\\data\\CorpusDocs")
          }
        }
        if(!file.exists("./data/OutputFreqs")){
          if (Sys.info()[['sysname']]!="Windows"){

            system("mkdir ./data/OutputFreqs")
          }else{
            shell("mkdir .\\data\\OutputFreqs")
          }
        }
        if(!file.exists("./data/Outputs")){
          if (Sys.info()[['sysname']]!="Windows"){

            system("mkdir ./data/Outputs")
          }else{
            shell("mkdir .\\data\\Outputs")
          }
        }



        CorpusDir <- gsub('"',"",CorpusDir)

        fileExt <- unique(gsub("^.*?\\.([A-z]{3,8})$", "\\1", dir(CorpusDir)))



        if(fileExt %in% c("vert","vrt","cqp")){

          source("./R/VertToDF.R")

          CorpusDerivedMeta <- lapply(dir(CorpusDir),
                                      function(x)
                                        MakeCorpusTablesFromVertAndReturnMeta(HeadwordVar,
                                                                              CorpusDir,
                                                                              x,
                                                                              columnNames,
                                                                              SentEndMarker,
                                                                              TagsVector,
                                                                              MetadataTag,
                                                                              TargetDir="./data",
                                                                              Cores=Cores))



          CorpusDerivedMeta <- CorpusDerivedMeta[!sapply(CorpusDerivedMeta, is.null)]
          if(!is.null(CorpusDerivedMeta) && length(CorpusDerivedMeta)>0){
            CorpusDerivedMeta <- do.call(rbind, CorpusDerivedMeta)
            if(is.data.frame(CorpusDerivedMeta)){
              write.fst(CorpusDerivedMeta, paste0("./data/CorpusData/",CorpusName,"_CorpusDerivedMeta.fst"))
              fwrite(CorpusDerivedMeta, paste0("./data/Outputs/",CorpusName,"_CorpusDerivedMeta.csv"))

            }
          }

        }else if(fileExt=="conllu"){


          source("./R/ConlluToDF.R")





          if (Sys.info()[['sysname']]!="Windows"){
            CorpusDerivedMeta <-  mclapply(dir(CorpusDir), function(x) CorpusConlluToDF(HeadwordVar,
                                                                                        CorpusDir,
                                                                                        x,
                                                                                        columnNames,
                                                                                        SentenceBoundaryMarker,
                                                                                        SentenceIDtag,
                                                                                        MetadataTag,
                                                                                        TargetDir="./data"),
                                           mc.cores=Cores)

          }else{



            cl <- makeCluster(as.numeric(Cores))
            print(as.numeric(Cores))

            clusterExport(cl, c('Cores'))
            clusterEvalQ(cl, {

              source("./R/VertToDF.R")
              source("./R/SetUp.R")
              source("./R/ConlluToDF.R")
              source("./R/CorpusFreqsAndDPfunctions.R")
              source("./R/flextextToDF.R")
              source("./R/FunctionToCreateCorpusObjects.R")
              source("./R/TxtToDF.R")

            })

            CorpusDerivedMeta <-  parLapply(cl,dir(CorpusDir), function(x) CorpusConlluToDF(HeadwordVar,
                                                                                            CorpusDir,
                                                                                            x,
                                                                                            columnNames,
                                                                                            SentenceBoundaryMarker,
                                                                                            SentenceIDtag,
                                                                                            MetadataTag,
                                                                                            TargetDir="./data"))
            stopCluster(cl)

          }





          CorpusDerivedMeta <- CorpusDerivedMeta[!sapply(CorpusDerivedMeta, is.null)]

          if(!is.null(CorpusDerivedMeta) && length(CorpusDerivedMeta)>0){

            CorpusDerivedMeta <- do.call(rbind, CorpusDerivedMeta)
            if(is.data.frame(CorpusDerivedMeta)){
              write.fst(CorpusDerivedMeta, paste0("./data/CorpusData/", CorpusName,"_CorpusDerivedMeta.fst"))
              fwrite(CorpusDerivedMeta, paste0("./data/Outputs/",CorpusName,"_CorpusDerivedMeta.csv"))

            }
          }

        }else if(fileExt=="csv"){


          source("./R/FunctionToCreateCorpusObjects.R")



          if (Sys.info()[['sysname']]!="Windows"){
            CorpusDerivedMeta <-  mclapply(dir(CorpusDir), function(x) MakeCorpusTables(HeadwordVar,
                                                                                        CorpusDir,
                                                                                        x,
                                                                                        SentEndMarker,
                                                                                        ColWithSentMarker,
                                                                                        MetadataTag,
                                                                                        ColWithMetadataTag,
                                                                                        TargetDir="./data"),
                                           mc.cores=Cores)

          }else{



            cl <- makeCluster(as.numeric(Cores))
            print(as.numeric(Cores))

            clusterExport(cl, c('Cores'))
            clusterEvalQ(cl, {

              source("./R/VertToDF.R")
              source("./R/SetUp.R")
              source("./R/ConlluToDF.R")
              source("./R/CorpusFreqsAndDPfunctions.R")
              source("./R/flextextToDF.R")
              source("./R/FunctionToCreateCorpusObjects.R")
              source("./R/TxtToDF.R")

            })

            CorpusDerivedMeta <-  parLapply(cl, dir(CorpusDir), function(x) MakeCorpusTables(HeadwordVar,
                                                                                             CorpusDir,
                                                                                             x,
                                                                                             SentEndMarker,
                                                                                             ColWithSentMarker,
                                                                                             MetadataTag,
                                                                                             ColWithMetadataTag,
                                                                                             TargetDir="./data"))
            stopCluster(cl)

          }





          CorpusDerivedMeta <- CorpusDerivedMeta[!sapply(CorpusDerivedMeta, is.null)]

          if(!is.null(CorpusDerivedMeta) && length(CorpusDerivedMeta)>0){
            CorpusDerivedMeta <- do.call(rbind, CorpusDerivedMeta)
            write.fst(CorpusDerivedMeta, paste0("./data/CorpusData/", CorpusName,"_CorpusDerivedMeta.fst"))
            fwrite(CorpusDerivedMeta, paste0("./data/Outputs/",CorpusName,"_CorpusDerivedMeta.csv"))

          }

        }else if(fileExt=="txt"){
          source("./R/TxtToDF.R")
          ConvertTxtCorpus(Language,CorpusDir, SentEndMarker, TargetDir="./data", preprocessedTargetDir)
          # no metadata extraction from txt !
        }else if(fileExt=="flextext"){
          source("./R/flextextToDF.R")

          CorpusDerivedMeta <- ConvertFlexToCorpus(HeadwordVar, CorpusDir,TargetDir="./data", preprocessedTargetDir)
          #print(CorpusDerivedMeta)
          if(!is.null(CorpusDerivedMeta) && length(CorpusDerivedMeta)>0){
            CorpusDerivedMeta <- do.call(rbind, CorpusDerivedMeta)
            write.fst(CorpusDerivedMeta, paste0("./data/CorpusData/", CorpusName,"_CorpusDerivedMeta.fst"))
            fwrite(CorpusDerivedMeta, paste0("./data/Outputs/",CorpusName,"_CorpusDerivedMeta.csv"))

          }
        }

      }


      # extract Freq Info

      source("./R/CorpusFreqsAndDPfunctions.R")

      NormalizeFreqPer <- Uploaded$NormalizeFreqPer[2]
      if(!is.null(NormalizeFreqPer)){
        NormalizeFreqPer <- as.numeric(NormalizeFreqPer)
      }else{
        NormalizeFreqPer <- 100000 # defaults to 100k
      }

      HeadwordFreqs <- GetHeadwordsFreqsDF(HeadwordVar, NormalizeFreqPer, "./data/OutputFreqs", dir("./data/OutputFreqs"), Cores=Cores)
      write.csv(HeadwordFreqs, "./data/Outputs/HeadwordFreqs.csv", row.names = F)



      output$done <- renderText({

        req(HeadwordFreqs)

        showModal(
          modalDialog(
            title = "done",
            "done! the frequency tables in are data > Outputs",
            easyClose = TRUE,
            footer = NULL
          )
        )

        paste("done!")

      })

    })

    observeEvent(input$SubcorporaCalc,{


      Uploaded <- read.csv("./data/dataExtractionInstructions.csv",stringsAsFactors = F)

      NormalizeFreqPer <- Uploaded$NormalizeFreqPer[2]
      if(!is.null(NormalizeFreqPer) && NormalizeFreqPer!=""){
        NormalizeFreqPer <- as.numeric(NormalizeFreqPer)
      }else{
        NormalizeFreqPer <- 100000 # defaults to 100k
      }

      if(is.null(input$MetaPath) || input$MetaPath==""){
        MetadataPath <- Uploaded$MetadataPath[2]
        if(MetadataPath==""){
          MetadataPath <- NULL
        }
        if(!is.null(MetadataPath) && length(MetadataPath)>0){
          MetadataPath <- gsub('"','',MetadataPath)
          MetadataPath <- gsub("'","",MetadataPath)
        }
      }else{
        MetadataPath <- input$MetaPath
      }

      if(is.null(input$SubcorpusVar) || input$SubcorpusVar==""){
        MetaVal <- Uploaded$MetaVar[2]
        if(MetaVal==""){
          MetaVal <- NULL
        }
        if(!is.null(MetaVal) && length(MetaVal)>0 &&  TRUE %in%  str_detect(MetaVal," ")){
          MetaVal <- gsub("^\\s+|\\s+$","", MetaVal)
          MetaVal <- gsub('"',"", MetaVal)
          MetaVal <- gsub("'","", MetaVal)
          MetaVal <- unlist(str_split(MetaVal," "))
        }
      }else{
        MetaVal <- input$SubcorpusVar
        if(!is.null(MetaVal) && length(MetaVal)>0 &&  TRUE %in%  str_detect(MetaVal," ")){
        MetaVal <- gsub("^\\s+|\\s+$","", MetaVal)
        MetaVal <- gsub('"',"", MetaVal)
        MetaVal <- gsub("'","", MetaVal)
        MetaVal <- unlist(str_split(MetaVal," "))
        }
      }


      for (i in 2:nrow(Uploaded)){
        HeadwordVar <- Uploaded$HeadwordVar[i]
        HeadwordVar <- gsub("^\\s|\\s$","",HeadwordVar)
        if(str_detect(HeadwordVar, "\\s")){
          HeadwordVar <- unlist(str_split(HeadwordVar,"\\s"))
        }
        MetadataTag <- Uploaded$MetadataTag[i]
        if(length(MetadataTag)==0 || MetadataTag==""){
          MetadataTag <- NULL
        }

        ColWithMetadataTag <- Uploaded$ColWithMetadataTag[i]
        if(ColWithMetadataTag==""){
          ColWithMetadataTag <- NULL
        }
      }

      ColToJoinBy <- "filename"


      Cores <<- as.numeric(Uploaded$Cores[2])
      if(Cores==""|is.na(Cores)){
        Cores <<- 2
      }

      print(Cores)

      if(!is.null(MetaVal)){
        source("./R/CorpusFreqsAndDPfunctions.R")
        SubcorporaFreqsAndDP <- GetSubcorporaFreqsAndDP(MetadataPath, ColToJoinBy, MetaVal,NormalizeFreqPer, HeadwordVar, FreqTablesSourceDir="./data/OutputFreqs", mcCores=Cores)
        write.csv(SubcorporaFreqsAndDP, paste0("./data/Outputs/SubcorporaHeadwordFreqsAndDP_",MetaVal,".csv"), row.names = F)


      }else{
        SubcorporaFreqsAndDP <- "nothing"
      }
      output$doneSubcorpora <- renderText({

        req(SubcorporaFreqsAndDP)

        showModal(
          modalDialog(
            title = "done",
            "done! the frequency tables in are data > Outputs",
            easyClose = TRUE,
            footer = NULL
          )
        )

        paste("done!")

      })

    })


    DictData <- reactive({
      if(!is.null(input$UploadDict)){
        if(str_detect(input$UploadDict$datapath, ".rds$")){
          readRDS(input$UploadDict$datapath)
        }else if(str_detect(input$UploadDict$datapath, ".csv$")){
          read.csv(input$UploadDict$datapath, stringsAsFactors = F)
        }else if(str_detect(input$UploadDict$datapath, "json$")){
          #jsonlite::read_json(input$UploadDict$datapath)
          jsonlite::fromJSON(input$UploadDict$datapath)

        }else{
          NULL
        }
      }else{
        NULL
      }
    })



    observeEvent(input$UploadDict, {

      DictData <- DictData()
      if(!is.null(DictData)){
        if(is.list(DictData) & !is.data.frame(DictData)){
         # NamesToFix <- names(DictData)[str_detect(names(DictData), "[^[[:lower:]][[:upper:]]\\d\\s-{}]")]
         #  if(length(NamesToFix)>0){
         #
         #  showModal(
         #    modalDialog(
         #      title = "special characters in headwords",
         #      paste("these special characters have been detected in data names and will be replaced with space: ",paste(NamesToFix, collapse = "    ")),
         #      easyClose = TRUE,
         #      footer = NULL
         #    )
         #  )
          # names(DictData) <- str_replace_all(names(DictData), "[^[[:lower:]][[:upper:]]\\d\\s-{}]"," ")
          # names(DictData) <- gsub("^\\s+|\\s+$","", names(DictData))
         # }

          saveRDS(DictData, "./data/dictData.rds")

          filenamingDF <- data.frame(headword=names(DictData), filenaming= do.call(c, lapply(names(DictData), function(x) MakeSafeForFilename(x))))
          write.csv(filenamingDF, "./filenamingDF.csv",row.names = F)
        }else if(is.data.frame(DictData)){
          write.csv(DictData, "./data/dictData.csv", row.names=F)

          filenamingDF <- data.frame(headword=DictData[,1], filenaming= do.call(c, lapply(DictData[,1], function(x) MakeSafeForFilename(x))))
          write.csv(filenamingDF, "./filenamingDF.csv",row.names = F)
        }
      }
    })



    observeEvent(input$lexicographR, {
      if(input$lexicographR=="Collocations"){
        if(file.exists("./data/dictData.rds")){
          DictData <- readRDS("./data/dictData.rds")
        }else if(file.exists("./data/dictData.csv")){
          DictData <- read.csv("./data/dictData.csv", stringsAsFactors = F)
        }else{
          DictData <- DictData()
        }
        CollocationsServer("collocations", DictData = DictData, HeadwordVar=read.csv("./data/dataExtractionInstructions.csv")$HeadwordVar[2], Cores = read.csv("./data/dataExtractionInstructions.csv")$Cores[2])
      }
    })

    observeEvent(input$lexicographR, {
      if(input$lexicographR=="Examples"){
        if(file.exists("./data/dictData.rds")){
          DictData <- readRDS("./data/dictData.rds")
        }else if(file.exists("./data/dictData.csv")){
          DictData <- read.csv("./data/dictData.csv", stringsAsFactors = F)
        }else{
          DictData <- DictData()
        }
        GdexSorterServer("gdexSampler", DictData = DictData, sIDindexDir="./data/CorpusData/", fileIdentifier="sIDindex_", HeadwordVar=read.csv("./data/dataExtractionInstructions.csv")$HeadwordVar[2], MetadataPath=read.csv("./data/dataExtractionInstructions.csv")$MetadataPath[2], Cores= read.csv("./data/dataExtractionInstructions.csv")$Cores[2])
      }
    })

    observeEvent(input$lexicographR, {
      if(input$lexicographR=="Visualizations"){

        if(length(dir("./data/Outputs/"))>0){
          VizDatasetChoices <- if(length(dir("./data/CorpusDocs"))>0){
            c(dir("./data/Outputs"), "CorpusData")
          }else{
            dir("./data/Outputs")
          }

          updateSelectInput(session, "VizDataset",
                            choices = VizDatasetChoices)
        }

        # VizDataset <- reactive({
        #   if(!is.null(input$VizDataset) && input$VizDataset!=""){
        #     if(input$VizDataset=="CorpusData"){
        #       read.fst(paste0("./data/CorpusDocs/", dir("./data/CorpusDocs")[1]))
        #     }else{
        #       if(str_detect(as.character(input$VizDataset), "csv$")){
        #       read.csv(paste0("./data/Outputs/", as.character(input$VizDataset)), stringsAsFactors = F)
        #      }
        #     }
        #   }else{
        #     NULL
        #   }
        # })

        if(file.exists("./data/dataExtractionInstructions.csv") ){
          observeEvent(c(input$VizDataset,input$chartType),{

            if(!is.null(input$VizDataset) && input$VizDataset!=""){
              if(file.exists("./data/dictData.rds")){
                DictData <- readRDS("./data/dictData.rds")
              }else if(file.exists("./data/dictData.csv")){
                DictData <- read.csv("./data/dictData.csv", stringsAsFactors = F)
              }else{
                DictData <- DictData()
              }

              # if(!is.null(input$VizDataset) && input$VizDataset!=""){
              #   if(input$VizDataset=="CorpusData"){
              #     VizDataset <-  read.fst(paste0("./data/CorpusDocs/", dir("./data/CorpusDocs")[1]))
              #   }else{
              #     if(str_detect(as.character(input$VizDataset), "csv$")){
              #     VizDataset <-    read.csv(paste0("./data/Outputs/", as.character(input$VizDataset)), stringsAsFactors = F)
              #     }
              #   }
              # }else{
              #   VizDataset <- NULL
              # }

              # print(paste(input$VizDataset))
              # print(paste("colnames(VizDataset) MyApp line 757" ,paste(colnames(VizDataset)[1:5]),collapse=" "))


              DataVizServer("dataviz", DictData = DictData, input$VizDataset, chartType=input$chartType, HeadwordVar=read.csv("./data/dataExtractionInstructions.csv")$HeadwordVar[2], read.csv("./data/dataExtractionInstructions.csv")$Cores[2])
            }else{
              print(" no input$VizDataset")
            }
          })

        }else{
          print("please upload instruction data")
        }

          observeEvent(input$NetworkType,{
            if(file.exists("./data/dictData.rds")){
              cores <- if(file.exists("./data/dataExtractionInstructions.csv")){
                read.csv("./data/dataExtractionInstructions.csv")$Cores[2]
              }else{
                cores <- 2
              }
              DictVizServer("dictviz", chartType=input$NetworkType, Cores= cores)
            }else{
              print("no rds dictionary data: please upload it from data-extraction tab")
            }
          })

      }
    })

    observeEvent(input$lexicographR, {
      if(input$lexicographR=="BuildDictionary"){

        QuartoEditorServer("editor", Cores= ifelse(file.exists("./data/dataExtractionInstructions.csv"), read.csv("./data/dataExtractionInstructions.csv")$Cores[2], 1))

      }
    })


    MakeSharableDataServer("share", Cores= ifelse(file.exists("./data/dataExtractionInstructions.csv"), read.csv("./data/dataExtractionInstructions.csv")$Cores[2], 1))

  }
  shinyApp(ui, server, options = list(launch.browser = TRUE))

}

