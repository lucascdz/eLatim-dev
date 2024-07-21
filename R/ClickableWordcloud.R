

ClickableWordcloudUI <- function(id) {
  tagList(
    wordcloud2Output(
      NS(id, "ClickableWordcloud") ),
  actionButton(
    NS(id, "doAll"),
    "plot all headwords: SLOW, use with caution!"),
  # checkboxInput(
  #   NS(id, "doAll"),
  #   "plot all headwords: SLOW, use with caution!", value=F)
  )
}

ClickableWordcloudServer <- function(id,HeadwordVec, DF, FullData,Cores) {
  moduleServer(id, function(input, output, session) {

        output$ClickableWordcloud <-  renderWordcloud2(wordcloud2a(DF) )

    observeEvent( input$doAll,{
      if(input$doAll==T){
      if(!file.exists("./data/Plots")){
        system("mkdir ./data/Plots")
      }
        if(!file.exists("./data/AltText")){
          system("mkdir ./data/AltText")
        }
    # mclapply(HeadwordVec, function(x) writeWC(FullData[FullData$lemma==x,], x ), mc.cores=Cores )

     lapply(HeadwordVec, function(x)  writeWC(FullData[FullData$lemma==x,], x ))


showModal(
  modalDialog(
    title = "done",
    "done! the wordclouds are in data > Plots",
    easyClose = TRUE,
    footer = NULL
  )
  )

updateCheckboxInput(session,"doAll", value=F)
}
    })

  })
}

