# radial DT chart


# UI

D3NetworkUI <- function(id, NetworkType="radial") {
  tagList(
    actionButton(NS(id,"test"),"test on a random case"),

    if (NetworkType=="radial"){
      networkD3::radialNetworkOutput(NS(id, "RadialGraph"))

    }else{
      networkD3::diagonalNetworkOutput(NS(id, "RadialGraph"))
    }
    ,
    #actionBttn(NS(id,"doAll"),"process all")
    checkboxInput(NS(id,"doAll"),"process all", value=F)

  )
}


# SERVER


D3NetworkServer <- function(id, Dataset, valsVec, NetworkType="radial",size=12, color="steelblue", Cores) { # order valsVec from most general to most specific
  moduleServer(id, function(input, output, session) {

      rootString <- valsVec[1]

      if(nrow(Dataset)>0){
        networkData <- Dataset[,colnames(Dataset) %in% valsVec]
        networkData[is.na(networkData)] <- "not available"

        observeEvent(input$test,{
          RandoVal <- sample(unique(Dataset[[ valsVec[1] ]]),1)
          networkData <- networkData[networkData[[ valsVec[1] ]]==RandoVal,]
          networkData <- mutate_if(networkData,
                                   is.character,
                                   str_replace_all, pattern = "/.*?$", replacement = "") # `/` conflicts with DT, so everything after  / in a string gets deleted
          networkData <- mutate_if(networkData,
                                   is.character,
                                   str_replace_all, pattern = "^name$", replacement = "Name") # to avoid namespace conflict with DT network

          networkData$root <- rep(rootString, nrow(networkData))
          valsVecWroot <- c("root", valsVec)

          networkData$pathString <- ""
          for (i in valsVecWroot){
            networkData$pathString <- paste(networkData$pathString, networkData[[i]],
                                            sep="/")
          }
          networkData <- as.Node(networkData)
          networkData <- as.list(networkData, mode = 'explicit', unname = TRUE)

          if (NetworkType=="radial"){
            output$RadialGraph <- renderRadialNetwork( radialNetwork(networkData,fontSize = size,nodeStroke = color) )

          }else{
            output$RadialGraph <- renderDiagonalNetwork( radialNetwork(networkData,fontSize = size,nodeStroke = color) )

          }
        })

        observeEvent(input$doAll,{
if(input$doAll==T){
       MakeAllStaticNetworks(Dataset, valsVec,NetworkType, color, fontSize, TargetDir="/data/Plots",Cores)
updateCheckboxInput(session,"doAll", value=F)
          }
        })
      }
  })
}

createNetwork <- function(networkData, valsVec, networkType="diagonal", TargetDir="./data/Plots"){
  rootString <- unique(networkData$lemma)


  networkData <- mutate_if(networkData,
                           is.character,
                           str_replace_all, pattern = "/.*?$", replacement = "") # `/` conflicts with DT, so everything after  / in a string gets deleted
  networkData <- mutate_if(networkData,
                           is.character,
                           str_replace_all, pattern = "^name$", replacement = "Name") # to avoid namespace conflict with DT network

  networkData$root <- rep(rootString, nrow(networkData))
  valsVecWroot <- c("root", valsVec)

  networkData$pathString <- ""
  for (i in valsVecWroot){
    networkData$pathString <- paste(networkData$pathString, networkData[[i]],
                                    sep="/")
  }
  networkData <- as.Node(networkData)
  networkData <- as.list(networkData, mode = 'explicit', unname = TRUE)
  if(networkType=="diagonal"){
    plot <- diagonalNetwork(networkData,fontSize = 12)
  }else{
    plot <- radialNetwork(networkData,fontSize = 12)
  }
  saveWidget(plot,"NetworsTmp.html",selfcontained = F)
  webshot("NetworsTmp.html",paste0(TargetDir,"/Network_",valsVec[1],"_IS",MakeSafeForFilename(rootString),"_BY_",paste(valsVec,collapse = "_"),".png"), delay =5, vwidth = 480, vheight=480) # changed to png.

}

