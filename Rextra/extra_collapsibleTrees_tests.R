library(tidyverse)
library(collapsibleTree)
library(htmlwidgets)

# functions
SafeEncodeUTF8 <- function(string){
   paste(utf8ToInt(string), collapse = "-")
}
MakeSafeForFilename <- function(string){
   paste0("_xx",SafeEncodeUTF8(string), "xx_")
}
MakeTree <- function(TreeDF, HierarchicalValsVec, rootVal, TargetDir=NULL, Interactive="interactive"){

   Tree <-  collapsibleTree(TreeDF,
                            hierarchy = c(as.character(HierarchicalValsVec)),
                            root =  as.character(rootVal), #
                            #attribute = "n_lex",
                            #nodeSize = "hits",
                            fontSize= 20,
                            #tooltip=T,
                            collapsed = T,
                            width = 800,
                            height = 600,
                            zoomable = T
   )

   if(!is.null(TargetDir)){
      if(Interactive=="interactive"){
         saveWidget(Tree,paste0(TargetDir,"/Tree_",MakeSafeForFilename(rootVal),"_",paste(HierarchicalValsVec,collapse = "_"),".html"), selfcontained = T)
      }else{
         saveWidget(Tree,"tmp.html",selfcontained = F)
         webshot("tmp.html",paste0(TargetDir,"/Tree_",MakeSafeForFilename(rootVal),"_BY_",paste(HierarchicalValsVec,collapse = "_"),".png"), delay =5, vwidth = 480, vheight=480) # changed to png.
      }
   }else{
      Tree
   }

}

# arguments
WordnetDF <- read.csv('/Users/lucascdz/Rprojects/iLexicographR/data/LexicalData_top999/lexidata_wordnetLexemes.csv')
WordnetList <- split(WordnetDF,WordnetDF$lemma)

# test
{ print(names(WordnetList[545])) ;
   MakeTree(TreeDF = WordnetList[[545]],
            HierarchicalValsVec = c('sem_type','lexemes_pt'),
            rootVal = names(WordnetList[545]),
            TargetDir = './data/Plots') }

# make trees for all lemmas
lapply(seq_along(WordnetList), function(i) {
   print(names(WordnetList[i])) ; MakeTree(TreeDF = WordnetList[[i]],
                                           HierarchicalValsVec = c('sem_type','lexemes_pt'),
                                           rootVal = names(WordnetList[i]),
                                           TargetDir = './data/Plots') } )
