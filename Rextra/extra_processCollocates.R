library(tidyverse)
{
   #### get filepaths ####
   output_folder <- './data/Outputs/'
   extraction_pattern <- 'CollocateCandidates_MinFreq10minLL5minLR0minMI0minLogDice0_win1_by'
   filenames <- dir(output_folder)[str_detect(dir(output_folder),extraction_pattern)]
   filepaths <- unlist(lapply(seq_along(filenames), function(i) paste0(output_folder,filenames[i])))

   #### bind collocates ####
   CollocatesList <- lapply(seq_along(filepaths), function(i) read.csv(filepaths[i]))
   CollocatesDF <- do.call(rbind,CollocatesList)
   CollocatesDF <- CollocatesDF[!duplicated(CollocatesDF),]

   #### postag collocates ####
   # get postag from lila kb (via 'https://lila-erc.eu/sparql/')
   # script: 'PREFIX lila: <http://lila-erc.eu/ontologies/lila/> SELECT ?lila_id ?pos WHERE { ?lila_id lila:hasPOS ?pos . }'
   LilaPostagDF <- read.csv('./data/extra_lila_postag.csv')
   LilaPostagDF$lila_id <- gsub('http://lila-erc.eu/data/id/lemma/','lilaLemma:',LilaPostagDF$lila_id) %>%
      gsub('http://lila-erc.eu/data/id/hypolemma/','lilaIpoLemma:',.)
   LilaPostagDF$pos <-
      gsub('http://lila-erc.eu/ontologies/lila/proper_noun','nome próprio',LilaPostagDF$pos) %>%
      gsub('http://lila-erc.eu/ontologies/lila/noun','substantivo',.) %>%
      gsub('http://lila-erc.eu/ontologies/lila/adjective','adjetivo',.) %>%
      gsub('http://lila-erc.eu/ontologies/lila/verb','verbo',.) %>%
      gsub('http://lila-erc.eu/ontologies/lila/adverb','advérbio',.) %>%
      gsub('http://lila-erc.eu/ontologies/lila/interjection','interjeição',.) %>%
      gsub('http://lila-erc.eu/ontologies/lila/other','outro',.) %>%
      gsub('http://lila-erc.eu/ontologies/lila/coordinating_conjunction','conj.coord.',.) %>%
      gsub('http://lila-erc.eu/ontologies/lila/numeral','numeral',.) %>%
      gsub('http://lila-erc.eu/ontologies/lila/adposition','preposição',.) %>%
      gsub('http://lila-erc.eu/ontologies/lila/determiner','determinante',.) %>%
      gsub('http://lila-erc.eu/ontologies/lila/pronoun','pronome',.) %>%
      gsub('http://lila-erc.eu/ontologies/lila/subordinating_conjunction','conj.subord.',.) %>%
      gsub('http://lila-erc.eu/ontologies/lila/particle','partícula',.)

   #### get lemma keys ####
   LemmaKeysDF <- read.csv('./data/extra_lemma_keys.tsv',sep='\t')
   colnames(LemmaKeysDF)[which(colnames(LemmaKeysDF)=='key')] <- 'collocate'
   PosKeysDF <- left_join(LemmaKeysDF,LilaPostagDF)
   PosKeysDF <- PosKeysDF[,-which(colnames(PosKeysDF)=='lila_id')]

   #### get postagged collocate lists ####
   CollocatesWithPostagDF <- left_join(CollocatesDF,PosKeysDF)
   CollocatesWithPostagDF[is.na(CollocatesWithPostagDF)] <- ''
   CollocatesWithPostagDF <- CollocatesWithPostagDF[,c(1:2,length(CollocatesWithPostagDF),3:(length(CollocatesWithPostagDF)-1))]

   #### round numeric columns ####
   numeric_columns <- which(unlist(lapply(seq_along(CollocatesWithPostagDF), function(i) class(CollocatesWithPostagDF[,i])))=='numeric')
   for(x in numeric_columns){
      CollocatesWithPostagDF[x] <- round(CollocatesWithPostagDF[x],digits = 1)
   }
   write_csv(CollocatesWithPostagDF,paste0(output_folder,gsub('by$','',extraction_pattern),'postagged.csv'))

   # create scores
   CollocatesByPostagDF <- CollocatesWithPostagDF
   CollocatesByPostagDF$allMethods <- round(unlist(lapply(seq_along(CollocatesByPostagDF$lemma), function(i) sum(CollocatesByPostagDF[i,numeric_columns]))),digits = 1)
   {
      meiamediana <- median(CollocatesByPostagDF$allMethods,na.rm = T)*2/3
      mediana <- median(CollocatesByPostagDF$allMethods,na.rm = T)
      mediana2x <- median(CollocatesByPostagDF$allMethods,na.rm = T)*2
      mediana4x <- median(CollocatesByPostagDF$allMethods,na.rm = T)*4
      mediana8x <- median(CollocatesByPostagDF$allMethods,na.rm = T)*8
   }
   CollocatesByPostagDF$score <- 10
   CollocatesByPostagDF$score[which(CollocatesByPostagDF$allMethods > meiamediana)] <- 20
   CollocatesByPostagDF$score[which(CollocatesByPostagDF$allMethods > mediana2x)] <- 30
   CollocatesByPostagDF$score[which(CollocatesByPostagDF$allMethods > mediana4x)] <- 40
   CollocatesByPostagDF$score[which(CollocatesByPostagDF$allMethods > mediana8x)] <- 50

   # score for adjectives
   CollocatesByPostagDF$scoreADJ <- 0
   CollocatesByPostagDF$scoreADJ[CollocatesByPostagDF$pos=='adjetivo'] <- CollocatesByPostagDF$score[CollocatesByPostagDF$pos=='adjetivo']
   CollocatesByPostagDF$scoreSUBST <- 0
   CollocatesByPostagDF$scoreSUBST[CollocatesByPostagDF$pos=='substantivo'] <- CollocatesByPostagDF$score[CollocatesByPostagDF$pos=='substantivo']
   CollocatesByPostagDF$scoreADV <- 0
   CollocatesByPostagDF$scoreADV[CollocatesByPostagDF$pos=='advérbio'] <- CollocatesByPostagDF$score[CollocatesByPostagDF$pos=='advérbio']
   CollocatesByPostagDF$scoreVERB <- 0
   CollocatesByPostagDF$scoreVERB[CollocatesByPostagDF$pos=='verbo'] <- CollocatesByPostagDF$score[CollocatesByPostagDF$pos=='verbo']
   CollocatesByPostagDF$scoreFUNC <- 0
   CollocatesByPostagDF$scoreFUNC[CollocatesByPostagDF$pos %in% c('conj.subord.','preposição')] <- CollocatesByPostagDF$score[CollocatesByPostagDF$pos %in% c('conj.subord.','preposição')]
   write_csv(CollocatesByPostagDF,paste0(output_folder,gsub('by$','',extraction_pattern),'scoreByPos.csv'))

   #CollocatesByPostagList <- split(CollocatesWithPostagDF,CollocatesWithPostagDF$pos)
   #for(i in seq_along(CollocatesByPostagList)){
   #   write_csv(CollocatesByPostagList[[i]],paste0(output_folder,gsub('by$','',extraction_pattern),'postagged_',names(CollocatesByPostagList)[i],'.csv'))
   #}
}
