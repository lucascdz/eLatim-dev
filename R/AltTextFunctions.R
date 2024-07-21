# functions to create AltText of plots generated in the app, for accessibility

# description for subcorpora freqs barcharts:
makeAltText_barchartsSubcorpora <- function(DF,xInput,fillInput, LEMMA){
  # makes description for barcharts based on subcorpora freqs datasets:

  AltText <- paste0("This charts plots the frequency of ",xInput ," by ",fillInput  ,". The ",DF$SubcorpusValue[which.max(DF$NormFreq)], " subcorpus registers the highest normalized frequency, with the value of ", round(DF$NormFreq[which.max(DF$NormFreq)],2) , " and an absolute frequency of ", DF$Freq[which.max(DF$NormFreq)],
                    ". The ", DF$SubcorpusValue[which.max(DF$NormFreq[-which.max(DF$NormFreq)])]
                    , " subcorpus follows, with a normalized frequency of ",round(DF$NormFreq[which.max(DF$NormFreq[-which.max(DF$NormFreq)])], 2)
                    , " and an absolute frequency of ", round(DF$Freq[which.max(DF$NormFreq[-which.max(DF$NormFreq)])], 2)
                    ,". the subcorpus with the least normalized frequency is ", DF$SubcorpusValue[which.min(DF$NormFreq)], " with the normalized value of ", round(DF$NormFreq[which.min(DF$NormFreq)],2), " and an absolute freqeuncy of ", round(DF$Freq[which.min(DF$NormFreq)],2), ". here are all the values: ",
                    paste(unlist(lapply(1:nrow(DF), function(i) paste("subcorpus:", DF[i,1],"; normalized frequency:",DF[i,2],"; absolute frequency:", DF[i,3]) )),collapse = ". ")
  )

  write(AltText, paste0("./data/AltText/barchart_",MakeSafeForFilename(LEMMA),"_",as.character(xInput),"_BY_",as.character(fillInput),"_AltText.txt"))
  return(AltText)
}

makeAltText_barchartsGeneric <- function(Dataset,xInput,fillInput, LEMMA){
  # makes description for barcharts that are not based on subcorpora freqs datasets:
  if(is.numeric(Dataset[[fillInput]])){
    DF <- Dataset[, c(which(colnames(Dataset)==xInput), which(colnames(Dataset)==fillInput) )]
    AltText <- paste0("This charts plots the frequency of ",xInput ," by ",fillInput,
                      " here are all the values plotted: ",
                      paste(unlist(lapply(1:nrow(DF), function(i) paste0(xInput, ": " , DF[i,1],"; ", fillInput, ": ",DF[i,2]) )), collapse = ". "))

  }else{
    DF <- as.data.frame(table(Dataset[[fillInput]]))
    AltText <- paste0("This charts plots the frequency of ",xInput ," by ",fillInput,
                      " here are all the values plotted: ",
                      paste(unlist(lapply(1:nrow(DF), function(i) paste0(fillInput, ": " , DF[i,1],"; frequency: ",DF[i,2]) )),collapse = ". ")
    )
  }

  write(AltText, paste0("./data/AltText/barchart_",MakeSafeForFilename(LEMMA),"_",as.character(xInput),"_BY_",as.character(fillInput),"_AltText.txt"))
  return(AltText)
}

makeAltText_logcurveFreq <- function(FreqsDF,freqCol, LEMMA){
  # makes description for frequency log curve
  orderedDF <- FreqsDF[order(FreqsDF[[freqCol]], decreasing = T),]
  orderedDF$rank <- 1:nrow(orderedDF)
  orderedDF$rank[orderedDF$lemma==LEMMA]

  AltText <- paste0("this chart shows that the frequency of ", LEMMA, " is ",  FreqsDF[[freqCol]][FreqsDF$lemma==LEMMA]," or, ",
                    round(FreqsDF[[freqCol]][FreqsDF$lemma==LEMMA]/sum(FreqsDF[[freqCol]])*10000,digits = 2),
                    " per 10000 words. By frequency, ", LEMMA, " ranks ", orderedDF$rank[orderedDF$lemma==LEMMA], "th out of ", nrow(orderedDF))


  write(AltText, paste0("./data/AltText/","Curve_",MakeSafeForFilename(LEMMA),"_",freqCol,"_AltText.txt"))
  return(AltText)
}

makeAltText_logcurveDP <- function(FreqsDF,freqCol, LEMMA){
  # makes description for dispersion logcurve
  orderedDF <- FreqsDF[order(FreqsDF[[freqCol]], decreasing = F),]
  orderedDF$rank <- 1:nrow(orderedDF)
  orderedDF$rank[orderedDF$lemma==LEMMA]

  AltText <- paste0("this chart shows that the dispersion of ", LEMMA, " by ",gsub("_DP$","", freqCol)," is ",  round(FreqsDF[[freqCol]][FreqsDF$lemma==LEMMA], 2 ),
                    ". ", LEMMA, " is the  ", orderedDF$rank[orderedDF$lemma==LEMMA], "th most evenly dispersed word by ",gsub("_DP$","", freqCol) ," in the corpus, out of ", nrow(orderedDF))

  write(AltText, paste0("./data/AltText/","Curve_",MakeSafeForFilename(LEMMA),"_",freqCol,"_AltText.txt"))
  return(AltText)
}

makeAltText_wordcloud <- function(DF, LEMMA){
  # makes description for wordcloud
  if(colnames(DF)[2]=="LR"){
    colnames(DF)[2] <- "Log-Ratio"
  }else if(colnames(DF)[2]=="LogLik"){
    colnames(DF)[2] <- "Log-Likelihood"
  }
  AltText <- paste0("This chart plots ", colnames(DF)[1], " by ", colnames(DF)[2], ", for the headword ",LEMMA, " here are all the values plotted: ",
                    paste(unlist(lapply(1:nrow(DF), function(i) paste0(colnames(DF)[1], ": " , DF[i,1],"; ",colnames(DF)[2],": ", round(DF[i,2], 2)) )),collapse = ". ")
  )
# revert to original col names for mathinkg filenames to plot
  if(colnames(DF)[2]=="Log-Ratio"){
    colnames(DF)[2] <- "LR"
  }else if(colnames(DF)[2]=="Log-Likelihood"){
    colnames(DF)[2] <- "LogLik"
  }
  write(AltText, paste0("./data/AltText/","wordcloud_", MakeSafeForFilename(LEMMA) , "_BY_" ,colnames(DF)[1],"_",colnames(DF)[2], "_AltText.txt"))
  return(AltText)
}

makeAltTextTrees <-function(HierarchicalValsVec, TreeDF, Val){
  AltTextTrees <- paste("tree chart showing hyerarchical relations between the variables: ",
                        paste(HierarchicalValsVec, collapse=", "), ". ",
                        paste(unlist(lapply(unique(TreeDF[[HierarchicalValsVec[1]]]),
                                            function(x) paste(colnames(TreeDF)[1], x, " includes ", colnames(TreeDF)[2],
                                                              paste(TreeDF[[colnames(TreeDF)[2] ]][TreeDF[[colnames(TreeDF)[1] ]]==x],collapse = ", "), collapse=". ")
                        )), collapse=". "))

  write(AltTextTrees , paste0("./data/AltText//Tree_",MakeSafeForFilename(Val),"_BY_",paste(HierarchicalValsVec,collapse = "_"),"_AltText.txt"))
  return(AltTextTrees)
}

makeAltTextNetwork <- function(valsVec, VAL){
  AltTextNetwork <- paste("network chart of ", VAL, "by ", paste(valsVec,collapse = ", ") )

  write(AltTextNetwork, paste0("./data/AltText/Network_",valsVec[1],"_IS",MakeSafeForFilename(VAL),"_BY_",paste(valsVec,collapse = "_"),"_AltText.txt") )
  return(AltTextNetwork)
}
