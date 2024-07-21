
GetSentLengthStats <- function(sIDindexDir="./data/CorpusData/", fileIdentifier="sIDindex_"){
# finds mean & median sentence length in a corpus using sentence index files

sIndexes <- lapply(dir(sIDindexDir)[str_detect(dir(sIDindexDir),fileIdentifier)],
                   function(x) read.fst(paste0(sIDindexDir,x) ))
sIndexesDF <- do.call(rbind, sIndexes)

return(data.frame(
       medianLength= round(median((sIndexesDF$End+1)-sIndexesDF$Start)),
       meanLength= round(mean((sIndexesDF$End+1)-sIndexesDF$Start)),
       minLength = min((sIndexesDF$End+1)-sIndexesDF$Start),
       maxLength = max((sIndexesDF$End+1)-sIndexesDF$Start)
       ))
}


#lenstats <- GetSentLengthStats()

