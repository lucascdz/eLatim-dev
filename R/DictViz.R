
DictVizUI <- function(id){
  tagList(
 column(width = 4,
        tags$div("visualise dictionary data (requires rds dictionary data, if you never uploaded it, go to home tab and upload it)"),
        tags$hr(),
        htmlOutput(NS(id,"availableVals")),
        tags$hr(),
        textInput(NS(id, "valsVec"), "list 2-3 space-separated variables in hierarchical order. variable MUST be in the list above"),
        actionBttn(NS(id,"go"),"go")
   ),
  column(width=8,
       uiOutput(NS(id,"DictDataViz"))
   )
 )
}



DictVizServer <- function(id,chartType,Cores){
  moduleServer(id, function(input, output, session) {

    if(file.exists("./data/dictData.rds")){

      DATA <- readRDS("./data/dictData.rds")

  output$availableVals <- renderText({
        paste0("available variables:<br /> ",
               paste(names(DATA[1][[1]]),collapse = ", " )
        )
    })

  observeEvent(input$go,{

    if(!is.null(input$valsVec) && str_detect(input$valsVec, "[[:lower:]]|[[:upper:]]")){
      valsVec <- unlist(str_split(as.character(input$valsVec), "\\s+"))
      valsVec <- valsVec[valsVec!=""]

      DF <- GetValsDFfromNamedList(DATA, valsVec)

      if(chartType=="tree"){
        output$DictDataViz <- renderUI({
          TreesUI(NS(id,"TREE"))
        })
        TreesServer("TREE", Dataset=DF, valsVec=valsVec, Cores=Cores)
      }else if(chartType=="diagonalNetwork"){
        output$DictDataViz <- renderUI({
          D3NetworkUI(NS(id,"diaNet"),NetworkType="diagonal")
        })
        D3NetworkServer("diaNet",Dataset=DF,valsVec=valsVec, NetworkType="diagonal",Cores=Cores)
      }else{
        output$DictDataViz <- renderUI({
          D3NetworkUI(NS(id,"radNet"),NetworkType="radial")
        })
        D3NetworkServer("radNet",Dataset=DF,valsVec=valsVec, NetworkType="radial",Cores=Cores)
      }

      # showModal(
      #   modalDialog(
      #     title = "done",
      #     "done! the graphs are in data > Plots",
      #     easyClose = TRUE,
      #     footer = NULL
      #   )
      # )

    }
  })
   }
 })
}
