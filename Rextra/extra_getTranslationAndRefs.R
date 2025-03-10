
GetTranslationsAndRefs <- function(DataFolder,SentTransDF,SentRefsDF){

   # get IDs
   CorpusDocsPath <- paste0(DataFolder,'CorpusDocs/')
   CorpusDocsFileVec <- dir(CorpusDocsPath)
   SentenceList <- lapply(seq_along(CorpusDocsFileVec), function(i) fst::read_fst(paste0(CorpusDocsPath,CorpusDocsFileVec[i])))
   SentenceList <- lapply(seq_along(SentenceList), function(i) data.frame(sent_id=unique(SentenceList[[i]]$sent_id),sID=unique(SentenceList[[i]]$sID),filename=gsub('\\.fst','',CorpusDocsFileVec[[i]]),stringsAsFactors = F))
   SentenceDF <- do.call(rbind,SentenceList)
   SentenceDF$ID <- unlist(lapply(seq_along(SentenceDF$sent_id), function(i) paste0(SentenceDF$sID[i],'___',SentenceDF$filename[i])))

   # get translations
   SentTransDF <- left_join(SentTransDF,SentenceDF)
   SentTransDF <- SentTransDF[,colnames(SentTransDF) %in% c('ID','translation_JDD','tradR')]

   # get refs
   SentRefsDF <- left_join(SentenceDF,SentRefsDF)
   SentRefsDF <- SentRefsDF[,colnames(SentRefsDF) %in% c('ID','ref')]

   # join to examples

   OutputsFolder <- paste0(DataFolder,'Outputs/')
   ExampleSentsDF <- read.csv(paste0(OutputsFolder,'ExampleSents.csv'))
   ExampleSentsDF$Sent <- gsub(' que ','que ',ExampleSentsDF$Sent) %>%
      gsub(' ue ','ue ',.)
   ExampleSentsDF <- left_join(ExampleSentsDF,SentRefsDF)
   ExampleSentsDF <- left_join(ExampleSentsDF,SentTransDF)
   # order and clear
   ExampleSentsDF <- ExampleSentsDF[order(ExampleSentsDF$translation_JDD),]
   ExampleSentsDF$translation_JDD <- gsub('[;:,] ?$','.',ExampleSentsDF$translation_JDD)
   write_csv(ExampleSentsDF,paste0(OutputsFolder,'ExampleSentsWithTranslation.csv'),na = '')

   return(print("Check the file 'ExampleSentsWithTranslation.csv' in ./data/Outputs folder."))

}
