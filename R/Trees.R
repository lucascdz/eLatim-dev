
TreesUI <- function(id){
  tagList(
    fluidRow(
    column(width=3,
           radioButtons(
             NS(id,"NodeSize"),
            "nodesize proportional to freq",
             choices=c("no","yes"))
           ),
    column(width=3,
           radioButtons(
             NS(id,"Collapsed"),
             "collapse",
             choices=c("no","yes"))
           ),
    column(width=3,
    actionButton(
      NS(id,"test"),
      "test on a random case")
           ),
),
fluidRow(
    collapsibleTreeOutput(NS(id,"Tree")),
),
fluidRow(
    column(width=3,
           radioButtons(NS(id,"Interactive"), "saveplot as", choices=c("interactive","static image"))),
    column(width=3,
           #actionBttn(NS(id,"doAll"),"process all")
           checkboxInput(NS(id,"doAll"),"process all",value=F)
           )
  )
)
}



TreesServer <- function(id, Dataset, valsVec, Cores){

  moduleServer(id, function(input, output, session) {

    observeEvent(input$test, {

      if(!is.null(valsVec) && is.vector(valsVec)){

      RandoVal <- sample(unique(Dataset[[ valsVec[1] ]]),1)
        output$Tree <- renderCollapsibleTree({
          MakeTree(Dataset, valsVec, Val=RandoVal,input$NodeSize, Collapsed=input$Collapsed, TargetDir = NULL, Interactive = input$Interactive)

        })
      }

    })

    observeEvent(input$doAll,{
      if(input$doAll==T){
        # lapply over top-hierarchy var and write to file
      NodeSize <- input$NodeSize
      Collapsed <- input$Collapsed
      Interactive <- input$Interactive
        MakeAllTrees(DF= Dataset, HierarchicalValsVec=valsVec, NodeSize=NodeSize, Collapsed=Collapsed, Interactive = Interactive, Cores=Cores)

     updateCheckboxInput(session, "doAll", value=F)
      }
    })


  })
}
