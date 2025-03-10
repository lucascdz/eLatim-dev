
# get uncommon and rare words (to be penalized) ####

## get LiLa KB lemma frequencies (via 'https://lila-erc.eu/sparql/')
## script: 'PREFIX lila: <http://lila-erc.eu/ontologies/lila/> PREFIX dc: <http://purl.org/dc/elements/1.1/> PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX powla: <http://purl.org/powla/powla.owl#> SELECT ?lila_id (count(?token) as ?freq) WHERE { ?token rdf:type powla:Terminal ; lila:hasLemma ?lila_id . } group by ?lila_id order by desc(?freq)'
LilaFreqsDF <- read.csv('./data/extra_lila_lemma_freq.csv')
LilaFreqsDF$lila_id <- gsub('http://lila-erc.eu/data/id/lemma/','lilaLemma:',LilaFreqsDF$lila_id) %>%
   gsub('http://lila-erc.eu/data/id/hypolemma/','lilaIpoLemma:',.)
LilaFreqsDF$freqRank <- seq(1:nrow(LilaFreqsDF))

## get lemma keys ####
LemmaKeysDF <- read.csv('./data/extra_lemma_keys.tsv',sep='\t')
colnames(LemmaKeysDF)[2] <- 'lemma'
LilaFreqsDF <- left_join(LilaFreqsDF,LemmaKeysDF)

## subset ####
threshold <- 5000
LemmaExclusionDF <- LilaFreqsDF[(threshold+1):nrow(LilaFreqsDF),colnames(LilaFreqsDF) %in% c('freqRank','lemma')]
write_csv(LemmaExclusionDF,paste0('./data/gdex_penalize_freqrank_over',threshold,'.csv'))



# get postag from lila kb (via 'https://lila-erc.eu/sparql/')
# script: 'PREFIX lila: <http://lila-erc.eu/ontologies/lila/> SELECT ?lila_id ?pos WHERE { ?lila_id lila:hasPOS ?pos . }'
LilaPostagDF <- read.csv('./data/extra_lila_postag.csv')
LilaPronounsDF <- LilaPostagDF[LilaPostagDF$pos=='http://lila-erc.eu/ontologies/lila/pronoun',]
LilaPronounsDF$lila_id <- gsub('http://lila-erc.eu/data/id/lemma/','lilaLemma:',LilaPronounsDF$lila_id) %>%
   gsub('http://lila-erc.eu/data/id/hypolemma/','lilaIpoLemma:',.)

# get lemma keys
LemmaKeysDF <- read.csv('./data/extra_lemma_keys.tsv',sep='\t')
colnames(LemmaKeysDF)[2] <- 'lemma'
LilaPronounsDF <- left_join(LilaPronounsDF,LemmaKeysDF)
LilaPronounsDF <- LilaPronounsDF[,c(1,3)]
write_csv(LilaPronounsDF,paste0('./data/gdex_penalize_pronouns.csv'))




