req_packages <- c("shiny",
                  "usethis",
                  "shinyjs",
                  "shinydashboard",
                  "shinyWidgets",
                  "readtext",
                  "markdown",
                  "fst",
                  "data.table",
                  "DT",
                  "tidyverse",
                  "wordcloud2",
                  "ggplot2",
                  "htmlwidgets",
                  "webshot",
                  "DT",
                  "plotly",
                  "RColorBrewer",
                  "collapsibleTree",
                  "data.tree",
                  "networkD3",
                  "quarto",
                  "pandoc",
                  "jsonlite",
                  "udpipe",
                  "textstem")


if(Sys.info()[['sysname']]!="Windows"){
  req_packages <- c(req_packages,"parallel")
}else{
  req_packages <- c(req_packages,"doParallel")
}

install.packages(setdiff(req_packages, rownames(installed.packages())), repos='http://cran.us.r-project.org', dependencies=TRUE)

if(Sys.info()[['sysname']]!="Windows"){
  require("parallel")
}else{
  require("doParallel")
}

require(shiny)
require(usethis)
require(shinyjs)
require(shinydashboard)
require(shinyWidgets)
require(readtext)
require(markdown)
require(data.table)
require(tidyverse)
require(fst)
require(wordcloud2)
require(ggplot2)
require(webshot)
require(htmlwidgets)
require(DT)
require(plotly)
require(RColorBrewer)
require(collapsibleTree)
require(networkD3)
require(data.tree)
require(quarto)
require(pandoc)
require(jsonlite)

if(!is_phantomjs_installed()){
webshot::install_phantomjs(force = TRUE)
}

if (Sys.info()[['sysname']]!="Windows"){
  #system("export QUARTO_DENO_EXTRA_OPTIONS=--inspect-brk")
  system("export QUARTO_DENO_EXTRA_OPTIONS=--v8-flags=--max-old-space-size=8192")
}else{
  #system('powershell.exe', input= "$env:QUARTO_DENO_EXTRA_OPTIONS = --inspect-brk")
  system('powershell.exe', input= "$env:QUARTO_DENO_EXTRA_OPTIONS = --v8-flags=--max-old-space-size=8192", intern=T)

}


JsonToList <- function(JsonPath){
  Json <- read_json("JsonPath")
  List <- lapply(seq_along(Json), function(i)  unlist(Json,recursive = F)) # if you need to unnest a level
  names(List) <- names(List)
  return(List)
}


ConvertDiacritics <- function(conversionTable, romanisedSktText){
  # the first column of conversionTable must have the character with diacritics,
  # the second column must have the corresponding caracter without diacritics

  for (i in 1:nrow(conversionTable)){
    romanisedSktText <- gsub(conversionTable[i,1], conversionTable[i,2], romanisedSktText)
  }

  return(romanisedSktText)
}


generateLogCurveGraph <- function(LEMMA, FreqsDF, freqCol, writeToFile="yes", MainPlot){


  # convert the lemma column to factor
if(!str_detect(freqCol,"_DP$")){
  AltText <-  makeAltText_logcurveFreq(FreqsDF, freqCol, LEMMA)

  LogCurveFreq <- MainPlot +
    geom_text(position=position_jitter(width=7,height=0), aes(label=ifelse(lemma %in% c(LEMMA),paste0( "  ",lemma," : ", FreqsDF[[freqCol]], " (", round(FreqsDF[[freqCol]]/sum(FreqsDF[[freqCol]])*10000,digits = 2), " per 10k words)"),''),hjust=0,vjust=0),color=ifelse(FreqsDF$lemma==LEMMA,"cornflowerblue","gray"),show.legend = FALSE)+
    labs( alt = AltText,
          caption= paste0(" max frequency: " , max(FreqsDF[[freqCol]]))) +
    theme(
      #plot.title = element_text(color="#C0C0C0", size=12),
      plot.caption = element_text(color="#C0C0C0", size=9),
    )

}else{
  AltText <- makeAltText_logcurveDP(FreqsDF, freqCol, LEMMA)
  LogCurveFreq <- MainPlot +
    geom_text(position=position_jitter(width=7,height=0), aes(label=ifelse(lemma %in% c(LEMMA),paste0( "  ",lemma," : ", FreqsDF[[freqCol]], " (", round(FreqsDF[[freqCol]]/sum(FreqsDF[[freqCol]])*10000,digits = 2), " per 10k words)"),''),hjust=0,vjust=0),color=ifelse(FreqsDF$lemma==LEMMA,"cornflowerblue","gray"),show.legend = FALSE)+
    theme(
      #plot.title = element_text(color="#C0C0C0", size=12),
      plot.caption = element_text(color="#C0C0C0", size=9),
    )+
    theme(axis.title.y = element_text(angle=90)) +
    labs(x= expression("more evenly distributed" %->% ""),
         y= expression("less evenly distributed" %->% ""),
         alt= AltText)
}

  # this plot takes into account thousands of lemma frequencies,
  # it takes a qhile to compute
  # if you want to use it in your dictionary it is best to save it,
  # and display the image in the dictionary, rather than calculate it online
  if( writeToFile=="yes"){
    ggsave(paste0("./data/Plots/","Curve_",MakeSafeForFilename(LEMMA),"_",freqCol,".png"), LogCurveFreq ,device="png")
  }else{
    LogCurveFreq
  }
}


writeWC <- function(df,Lem){

  df <- df[df$lemma==Lem,-1]
  print(Lem)
  if(!is.null(df) && is.data.frame(df) && nrow(df)>1){
    WC <- wordcloud2a(df)
    saveWidget(WC,"tmp.html",selfcontained = F)
    webshot("tmp.html",paste0("./data/Plots/","wordcloud_", MakeSafeForFilename(Lem) , "_BY_" ,colnames(df)[1],"_",colnames(df)[2], ".png"), delay =5, vwidth = 480, vheight=480) # changed to png.
    makeAltText_wordcloud(df, Lem)
  }
}


createTemplate <- function(){
  data.frame(CorpusName="input here your corpus name",
             CorpusDir="corpus filepath",
             columnNames="for vert and conllu: space-separated list of column names (must match the number of cols in the data)",
             HeadwordVar="Headword variable(s) e.g. lemma or lemma pos",
             SentEndMarker="for vert & csv: end of sentnece marker e.g. </s>. for txt the default is [.?!] input a string of space-separated characters to add more",
             ColWithSentMarker="only for csv column containing the SentEndMarker e.g. word)",
             TagsVector="TagsVector: (OPTIONAL) only for vert : comma separated list of all xml elements that need to be preserved in the corpus (excluding metadata) e.g. page",
             MetadataTag="OPTIONAL for conllu vert & csv: one xml or #-marked element containing metadata without the <> or # symbols, e.g. doc or source",
             ColWithMetadataTag="only for csv and vert: if MetadataTag is filled: name of column in which the metadata element is found",
             SentenceBoundaryMarker="only for conllu: the topmost #-marked element in a sentnece",
             SentenceIDtag=" only for conllu: #-marked element with unique sentnece id e.g. sent_id",
             Language="only for txt: lowercase name of the language of the corpus e.g. english",
             NormalizeFreqPer="input 10000 100000 or 1000000",
             Cores="number of cores you want to use to process the corpus if unsure leave blank. there are multiple parallelised operations. unless you have more than 20 cores available we recommend choosing a maximum of 4",
            # MetadataPath="path to a metadatafile. it must have a `filename` column with filenames matching the corpus filenames minus the file extension. only ONE metadatapath is accepted, if using multiple corpora assemble all metadata in the same file and harmonize the metadata tagset across corpora ",
            # MetaVar="the metadata variable to be used (e.g. domain or period)- avoid very granular category (e.g. title). for easily interpretable results the variable should have between 2 and 10 values. Metadata attributes derived from corpus files (via MetadataTag) can be used here, but they MUST BE THE SAME FOR ALL CORPORA USED (same value in all rows of this spreadsheet)",
             preprocessingTargetDir="OPTIONAL for txt and flextext only: path to directory where a preprocessed version of the corpus (csv/conllu) is to be written")


}

wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI",
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white",
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65,
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) {
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,",
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq,
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color,
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor,
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation,
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape,
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings,
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0,
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}




plotBarcharts <- function(Dataset, xInput,fillInput,barchartType,VarsAsShades,facetInput,xlabel="",ylabel="",LEMMA){

if(!is.null(Dataset) && nrow(Dataset)>0){
  if(str_detect(fillInput, "_Frequencies$") && unique(str_detect(colnames(Dataset)[2:length(Dataset)], "Freq$"))==TRUE){ # only applied to SubcorporaFreqs data
    if(colnames(Dataset)[1]=="lemma"){
      Dataset <- Dataset[,-1]
    }
    DF <- pivot_longer(Dataset,cols = everything(), names_to = c("SubcorpusValue","Freq"),names_sep = "_")
    DF <- pivot_wider(DF, names_from = Freq, values_from = value )

     altText <- makeAltText_barchartsSubcorpora(DF,xInput,fillInput, LEMMA)

    mainPlot <- ggplot(DF, aes(SubcorpusValue, NormFreq ,fill=Freq), color="blue")+geom_bar(stat="identity")+theme_minimal()+
      #labs(title = "Normalised frequency of prajñapti by period", caption="frequency normalized per 10k words") +
      theme(
        axis.title = element_text(colour = "gray"),
        axis.text.x = element_text(angle = 70, hjust = 1,colour = "gray"),
        axis.text.y = element_text(color="gray"),
        legend.title = element_text(color = "gray"),
        legend.text = element_text(color = "gray")
      ) +
      # xlab("")  +
      # ylab("normalized freq") +
      labs(
        x="",
        y ="normalized freq",
        alt= altText
      ) +
      geom_text(data=DF,aes(x=SubcorpusValue,y=NormFreq ,label=Freq),position = position_dodge(0.9), vjust=0, colour="#45464c",size=2) #+


  }else{ # all other charts

   AltText <- makeAltText_barchartsGeneric(Dataset,xInput,fillInput, LEMMA)

    if(is.numeric(Dataset[[fillInput]])){

      mainPlot <- ggplot(Dataset, aes_string(xInput, y= fillInput)) +
        geom_bar(stat="identity") + coord_flip()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              text=element_text(color="slategray",size=10))+
       # xlab(label = xlabel) +ylab(label=ylabel)
      labs(
        x= xlabel,
        y= ylabel,
        alt= AltText
      )
    }else{
      mainPlot <- ggplot(Dataset, aes_string(xInput, fill=fillInput)) +
        geom_bar(stat="count", position=barchartType) + coord_flip()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              text=element_text(color="slategray",size=10))+
        # xlab(label = xlabel) +ylab(label=ylabel)
        labs(
          x= xlabel,
          y= ylabel,
          alt= AltText
        )

      if (VarsAsShades=="shades"){

        if(length(unique(Dataset[[fillInput]]))<9){

          mainPlot <-  mainPlot + scale_fill_brewer()
        }else{
          mainPlot <-  mainPlot + scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Accent"))(length(unique(Dataset[[fillInput]]))))
        }
      }
    }

  }

  return(mainPlot)
}
}


SafeEncodeUTF8 <- function(string){
  paste(utf8ToInt(string), collapse = "-")
}

SafeDecodeUTF8 <- function(dashedNumstring){
  # dashedNumstring must be output of SafeEncodeUTF8, e.g "aṣṭāṅga" => "97-7779-7789-257-7749-103-97"
  intToUtf8(as.numeric(unlist(str_split(dashedNumstring,"-"))))
}

MakeSafeForFilename <- function(string){
  paste0("_xx",SafeEncodeUTF8(string), "xx_")
}

DecodeSafeFilename <- function(SafeFilename){
  string <- str_extract(SafeFilename, "_xx[\\d|-]+xx_")
  string <- gsub("_xx|xx_","",string)
  return(SafeDecodeUTF8(string))
}

PreviewDict <- function(){

  if(!str_detect(getwd(),"/MyDict$")){
  setwd("./MyDict/")
  }
  quarto_preview_stop()
  #getOption("viewer", utils::browseURL)
  quarto_preview()
  setwd("../")
}


