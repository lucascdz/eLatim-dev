
logcurveUI <- function(id){

  tagList(

  plotOutput(NS(id,"curve")),

  actionButton(
    NS(id, "doAll"),
    "plot all headwords: SLOW, use with caution!")
  )
}

logcurveServer <- function(id,HeadwordVec, Data, LEMMA, freqCol, MainPlot, Cores){
  moduleServer(id, function(input, output, session) {

    output$curve <-   renderPlot({
      generateLogCurveGraph(LEMMA, Data, freqCol, writeToFile="no",MainPlot)
    })

observeEvent(input$doAll,{
  if(!file.exists("./data/Plots")){
    system("mkdir ./data/Plots")
  }
  if(!file.exists("./data/AltText")){
    system("mkdir ./data/AltText")
  }
    #mclapply(HeadwordVec,  function(x) generateLogCurveGraph(x, Data, freqCol, writeToFile="yes",MainPlot),mc.cores = Cores)
    lapply(HeadwordVec,  function(x) generateLogCurveGraph(x, Data, freqCol, writeToFile="yes",MainPlot))

  showModal(
    modalDialog(
      title = "done",
      "done! the curve graphs are in data > Plots",
      easyClose = TRUE,
      footer = NULL
    )
  )
})

  })
}




