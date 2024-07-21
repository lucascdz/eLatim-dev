# library(tidyverse)
# dataDir <- "/Users/ligeialugli/Dropbox/DemocratizingDigitalLexy_Private/aaronAllFlextextConverted/aaron_data"
#
# dir(paste0(dataDir,"/Outputs"))
#
#
 #SubFr <- read.csv("./data/Outputs/SubcorporaHeadwordFreqsAndDP.csv")


# MetaVar <- "location" # parametrize this
#
#
# NormFreqsDF <- SubFr[ ,c(colnames(SubFr)[1],colnames(SubFr)[str_detect(colnames(SubFr),"NormFreq")])]
# colnames(NormFreqsDF) <- gsub("^(.*?)_NormFreq$","\\1",colnames(NormFreqsDF))
# NormFreqsDFLonger <- pivot_longer(NormFreqsDF,cols=!cf, names_to = MetaVar, values_to = "NormFreq" )
#
# FreqDF <- SubFr[ ,c(colnames(SubFr)[1],colnames(SubFr)[str_detect(colnames(SubFr),"_Freq")])]
# colnames(FreqDF) <- gsub("^(.*?)_Freq$","\\1",colnames(FreqDF))
# FreqDFLonger <- pivot_longer(FreqDF,cols=!cf, names_to = MetaVar, values_to = "Freq" )
#
# LongerDF <- cbind(FreqDFLonger, NormFreqsDFLonger[,3])
# colnames(LongerDF)
#write.csv(LongerDF, "./data/Outputs/SubcorpFreqsReshaped.csv",row.names = F)

