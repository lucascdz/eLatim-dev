library(tidyverse)

DataFolder <- './data/'
TranslationsDF <- read.csv('./data/extra_translations_JDD.tsv',sep='\t')
SentRefsDF <- read.csv('./data/extra_sentRefs.csv')
OutputsFolder <- paste0(DataFolder,'Outputs/')
ExamplesDF <- read.csv(paste0(OutputsFolder,'ExampleSents.csv'))

#GetTranslationsAndRefs <- function(DataFolder,SentTransDF,SentRefsDF){

# get IDs
CorpusDocsPath <- paste0(DataFolder,'CorpusDocs/')
CorpusDocsFileVec <- dir(CorpusDocsPath)
SentIDsList <- lapply(seq_along(CorpusDocsFileVec), function(i) fst::read_fst(paste0(CorpusDocsPath,CorpusDocsFileVec[i])))
SentIDsList <- lapply(seq_along(SentIDsList), function(i) data.frame(sent_id=unique(SentIDsList[[i]]$sent_id),sID=unique(SentIDsList[[i]]$sID),filename=gsub('\\.fst','',CorpusDocsFileVec[[i]]),stringsAsFactors = F))
SentIDsDF <- do.call(rbind,SentIDsList)
SentIDsDF$ID <- unlist(lapply(seq_along(SentIDsDF$sent_id), function(i) paste0(SentIDsDF$sID[i],'___',SentIDsDF$filename[i])))

# get refs
SentIDsWithRefsDF <- left_join(SentIDsDF,SentRefsDF)

# get translations
TranslationsDF$traducao <- gsub('[;:,] ?$','.',TranslationsDF$traducao)
SentIDsWithRefsAndTransDF <- left_join(SentIDsWithRefsDF,TranslationsDF)
SentIDsWithRefsAndTransDF <- SentIDsWithRefsAndTransDF[,colnames(SentIDsWithRefsAndTransDF) %in% c('ID','ref','traducao','tradutor')]

# join refs and translations to examples
ExamplesDF$Sent <- gsub(' que ','que ',ExamplesDF$Sent) %>%
   gsub(' ue ','ue ',.)
ExamplesDF$wordcount <- unlist(lapply(seq_along(ExamplesDF[,1]), function(i) str_count(ExamplesDF$Sent[i],' ')+1))
ExamplesDF <- ExamplesDF[order(ExamplesDF$wordcount),]
ExamplesDF$wordcount <- NULL

TranslatedExamplesDF <- left_join(ExamplesDF,SentIDsWithRefsAndTransDF)
TranslatedExamplesDF <- TranslatedExamplesDF[,c(which(colnames(TranslatedExamplesDF)=='lemma'),
                            which(colnames(TranslatedExamplesDF)=='ref'),
                            which(colnames(TranslatedExamplesDF)=='Sent'),
                            which(colnames(TranslatedExamplesDF)=='traducao'),
                            which(colnames(TranslatedExamplesDF)=='tradutor'))]
colnames(TranslatedExamplesDF) <- c('lemma','loc.cit.','texto','traducao','tradutor')
rownames(TranslatedExamplesDF) <- NULL

# save
write_csv(TranslatedExamplesDF,paste0(OutputsFolder,'ExampleSentsWithTranslation.csv'),na = '')


#}
