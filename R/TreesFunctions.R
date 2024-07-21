
GetValDFs <- function(List, VAL){
  ValDFs <- lapply(seq_along(List), function(i) if(!is.null(List[i][[1]][[VAL]]) && length(List[i][[1]][[VAL]])>0){print(i);data.frame(lemma=names(List[i]),VAL=List[i][[1]][[VAL]])}else{data.frame(lemma=names(List[i]),VAL="")})
  ValDF <- do.call(rbind,ValDFs)
  colnames(ValDF)[2] <- VAL
  return(ValDF)
}



GetValsDFfromNamedList <- function(List, valsVec){


  AllValDFs <-  lapply(valsVec, function(x) GetValDFs(List,x) )


  AllValDFs <- AllValDFs[!sapply(AllValDFs, is.null)]

  if(!is.null(AllValDFs)){
    if(length(unique(unlist(lapply(AllValDFs, function(x) nrow(x)))))==1){
    ValsDF <- do.call(cbind, AllValDFs)
    }else if(length(unique(AllValDFs[1][[1]]$lemma))==length(AllValDFs[1][[1]]$lemma)){
    ValsDF <- Reduce(function(x,y) full_join(x,y, by="lemma"),  AllValDFs)
    }else{
      stop("the selected variables are not amenable to this chart")
    }
    ValsDF <- ValsDF[,-1] # remove lemma col, which was created by the system and is related to not user-input
    if(length(which(str_detect(colnames(ValsDF), "^lemma\\.\\d+$")))>0){
      ValsDF <-  ValsDF[,-which(str_detect(colnames(ValsDF), "^lemma\\.\\d+$"))] # remove multiple lemma cols that are artifact of join
    }
    return(ValsDF)
  }
}

MakeTree <- function(DF, HierarchicalValsVec, Val, NodeSize="no", Collapsed="no", TargetDir=NULL, Interactive="interactive"){
  Collapsed <- ifelse(Collapsed=="no",FALSE,TRUE)

  if(NodeSize=="no"){
    TreeDF <-  DF[DF[[HierarchicalValsVec[1]]]==Val,] %>%
      group_by_at(colnames(DF)) %>%
      summarize("hits" = n())
    Tree <-  collapsibleTree(TreeDF,
        hierarchy = c(as.character(HierarchicalValsVec)[-1]),
        root =  as.character(Val), #
        attribute = "hits",
        #nodeSize = "hits",
        fontSize= 22,
        tooltip=T,
        collapsed = Collapsed,
        zoomable = T
      )
  }else{
    TreeDF <- DF[DF[[HierarchicalValsVec[1]]]==Val,] %>%
      group_by_at(colnames(DF)) %>%
      summarize("hits" = n())
    Tree <-  collapsibleTree(TreeDF,
        hierarchy = c(as.character(HierarchicalValsVec)[-1]),
        root =  as.character(Val), #
        attribute = "hits",
        nodeSize = "hits",
        fontSize= 22,
        tooltip=T,
        collapsed = Collapsed,
        zoomable = T
      )
  }

  if(!is.null(TargetDir)){
    if(Interactive=="interactive"){
      saveWidget(Tree,paste0(TargetDir,"/Tree_",MakeSafeForFilename(Val),"_",paste(HierarchicalValsVec,collapse = "_"),".html"), selfcontained = T)
    }else{
      saveWidget(Tree,"tmp.html",selfcontained = F)
      webshot("tmp.html",paste0(TargetDir,"/Tree_",MakeSafeForFilename(Val),"_BY_",paste(HierarchicalValsVec,collapse = "_"),".png"), delay =5, vwidth = 480, vheight=480) # changed to png.
    }
    if(file.exists("./data/AltText")){
    makeAltTextTrees(HierarchicalValsVec, TreeDF, Val)
    }
  }else{
    Tree
  }

}

MakeAllTrees <- function(DF, HierarchicalValsVec, NodeSize="no", Collapsed="no" , Interactive="interactive" ,Cores){
  if(!file.exists("./data/Plots/")){
    system("mkdir ./data/Plots/")
  }
  if(!file.exists("./data/AltText")){
    system("mkdir ./data/AltText")
  }
   #mclapply(unique(DF[[HierarchicalValsVec[1]]]), function(x) MakeTree(DF=DF, HierarchicalValsVec=HierarchicalValsVec, Val=x, NodeSize=NodeSize,Collapsed=Collapsed, TargetDir="./data/Plots/",Interactive=Interactive),mc.cores=Cores)
   # for testing:
   lapply(unique(DF[[HierarchicalValsVec[1]]]), function(x) {print(x);MakeTree(DF=DF, HierarchicalValsVec=HierarchicalValsVec, Val=x, NodeSize=NodeSize,Collapsed=Collapsed, TargetDir="./data/Plots/",Interactive=Interactive)})

  # showModal(
  #   modalDialog(
  #     title = "done",
  #     "done! the graphs are in data > Plots",
  #     easyClose = TRUE,
  #     footer = NULL
  #   )
  # )

}

PrepStaticNetworkData <- function(networkData, valsVec, rootString){
networkData <- mutate_if(networkData,
                         is.character,
                         str_replace_all, pattern = "/.*?$", replacement = "") # `/` conflicts with DT, so everything after  / in a string gets deleted
networkData <- mutate_if(networkData,
                         is.character,
                         str_replace_all, pattern = "^name$", replacement = "Name") # to avoid namespace conflict with DT network

networkData$root <- rep(rootString, nrow(networkData))
valsVec <- c("root", valsVec)

networkData$pathString <- ""
for (i in valsVec){
  networkData$pathString <- paste(networkData$pathString, networkData[[i]],
                                  sep="/")
}
networkData <- as.Node(networkData)
networkData <- as.list(networkData, mode = 'explicit', unname = TRUE)
return(networkData)
}


MakeStaticNetwork <- function(networkData, valsVec, NetworkType, color='steelblue', fontSize=12,TargetDir=""){
  Data <- PrepStaticNetworkData(networkData, valsVec, rootString=valsVec[1])
  if(NetworkType=="radial"){
  plot <- radialNetwork(Data,fontSize = fontSize, nodeStroke = color)
  }else{
  plot <- diagonalNetwork(Data,fontSize = fontSize, nodeStroke = color)
  }
  saveWidget(plot,"tmp.html",selfcontained = F)
  webshot("tmp.html",paste0(TargetDir,"/Network_",valsVec[1],"_IS",MakeSafeForFilename(VAL),"_BY_",paste(valsVec,collapse = "_"),".png"), delay =5, vwidth = 480, vheight=480) # changed to png.
 }

MakeStaticNetworkFromDF <- function(VAL, DF, valsVec, NetworkType, color='steelblue', fontSize=12,TargetDir=""){
  Data <- PrepStaticNetworkData(DF[DF[[valsVec[1]]]==VAL,], valsVec, rootString=VAL)
  if(NetworkType=="radial"){
    plot <- radialNetwork(Data,fontSize = fontSize, nodeStroke = color)
  }else{
    plot <- diagonalNetwork(Data,fontSize = fontSize, nodeStroke = color)
  }
  saveWidget(plot,"NetworsTmp.html",selfcontained = F)
  webshot("NetworsTmp.html",paste0(TargetDir,"/Network_",valsVec[1],"_IS",MakeSafeForFilename(VAL),"_BY_",paste(valsVec,collapse = "_"),".png"), delay =5, vwidth = 480, vheight=480) # changed to png.
  if(file.exists("./data/AltText")){
    makeAltTextNetwork(valsVec, VAL)
  }
}


MakeAllStaticNetworks <- function(DF, valsVec,NetworkType, color='steelblue', fontSize=12,TargetDir="",Cores){

  if(!file.exists("./data/Plots")){
    system("mkdir ./data/Plots")
  }
  if(!file.exists("./data/AltText")){
    system("mkdir ./data/AltText")
  }

      lapply(unique(DF[[valsVec[1]]]), function(x)  MakeStaticNetworkFromDF(VAL=x, DF=DF[DF[[valsVec[1]]]==x,], valsVec, NetworkType, color='steelblue', fontSize=12, TargetDir="./data/Plots"))

      lapply(unique(DF[[valsVec[1]]]), function(x)  MakeStaticNetworkFromDF(VAL=x, DF=DF[DF[[valsVec[1]]]==x,], valsVec, NetworkType, color='steelblue', fontSize=12, TargetDir="./data/Plots"))


  # showModal(
  #   modalDialog(
  #     title = "done",
  #     "done! the graphs are in data > Plots",
  #     easyClose = TRUE,
  #     footer = NULL
  #   )
  # )
}


