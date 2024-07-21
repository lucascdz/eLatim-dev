
PatternFinder <- function(SampleHeadwords, patternType, Data){
  SampleHeadwords <- Escape(SampleHeadwords)
  if(patternType=="before"){
    patterns <- unlist(lapply(SampleHeadwords, function(x) str_extract(Data, paste0(".{0,20}",x) ) %>% str_replace(.,x,"XXX") ) )
    patterns[is.na(patterns)] <- ""
    patterns <- patterns[patterns!=""]
    patterns <- patterns[nchar(patterns)==median(nchar(patterns))]
    if(length(patterns)>0){
      ratio <- length(unique(patterns))/length(patterns)
      if(ratio > 1/length(patterns)){
        while(ratio > 1/(length(patterns)) && length(patterns[nchar(patterns) < 5])==0){
          patterns <- unlist(lapply(patterns, function(x) substring(x, 2, nchar(x))))
          ratio <- length(unique(patterns))/length(patterns)
        }
      }
    }
  }else if(patternType=="after"){
    patterns <- unlist(lapply(SampleHeadwords, function(x) {print(x);pat <- str_extract(Data, paste0(x,".{0,20}") ) ; pat <- str_replace(pat[!is.na(pat)],x,"XXX"); print(pat);pat} ))
    patterns[is.na(patterns)] <- ""
    patterns <- patterns[patterns!=""]
    patterns <- patterns[nchar(patterns)==median(nchar(patterns))]
    if(length(patterns)>0){
      ratio <- length(unique(patterns))/length(patterns)
      if(ratio > 1/length(patterns)){
        while(ratio > 1/length(patterns) && length(patterns[nchar(patterns) < 5])==0){
          patterns <- unlist(lapply(patterns, function(x) substring(x, 1, nchar(x)-1 )))
          ratio <- length(unique(patterns))/length(patterns)
        }
      }
    }
  }
  if(length(unique(patterns))>1){
    patternsDF <- as.data.frame(table(patterns), stringsAsFactors = F)
    if(max(patternsDF$Freq)/sum(patternsDF$Freq) > 0.5){
      return(gsub("XXX","", patternsDF$patterns[which.max(patternsDF$Freq)]))
    }else{
      return("")
    }
  }else{
    if(length(patterns)>1){
      return(gsub("XXX","", unique(patterns)))
    }else{
      return("")
    }
  }
}

stringChrChecker <- function(SampleHeadwords,PAT){
  if (length(SampleHeadwords[str_detect(SampleHeadwords, PAT )])==length(SampleHeadwords)){
    MustContain <- PAT
  }else{
    MustContain <- 'any character'
  }
  if(length(SampleHeadwords[str_detect(SampleHeadwords, PAT )])==0){
    DoesNotContain <- PAT
  }else{
    DoesNotContain <- ""
  }
  return(data.frame(MustContain, DoesNotContain))
}
EntryElementExtractor <- function(SampleHeadwords,Data,ElementName){
  SampleHeadwords <- SampleHeadwords[SampleHeadwords!=""]

  lowercase <- "[[:lower:]]"
  uppercase <- "[[:upper:]]"
  punctuation <- "[\\.,;:\\!\\?]"
  number <- "\\d"


  chrchecks <- lapply(c(lowercase,uppercase,punctuation,number), function(x) stringChrChecker(SampleHeadwords,x))
  chrchecks <- do.call(rbind, chrchecks)

  chrchecks$DoesNotContain[chrchecks$DoesNotContain=="[[:lower:]]"] <- "lowercase"
  chrchecks$DoesNotContain[chrchecks$DoesNotContain=="[[:upper:]]"] <- "uppercase"
  chrchecks$DoesNotContain[chrchecks$DoesNotContain=="[\\.,;:\\!\\?]"] <- "punctuation"
  chrchecks$DoesNotContain[chrchecks$DoesNotContain=="\\d"] <- "number"

  chrchecks$MustContain[chrchecks$MustContain=="[[:lower:]]"] <- "lowercase"
  chrchecks$MustContain[chrchecks$MustContain=="[[:upper:]]"] <- "uppercase"
  chrchecks$MustContain[chrchecks$MustContain=="[\\.,;:\\!\\?]"] <- "punctuation"
  chrchecks$MustContain[chrchecks$MustContain=="\\d"] <- "number"

  if(length(unique(chrchecks$MustContain))>1){
    MustContain <- paste(unique(chrchecks$MustContain[chrchecks$MustContain!="any character"]), collapse = "AND")
  }else if(length(unique(chrchecks$MustContain[chrchecks$MustContain!="any character"]))==1){
    MustContain <- unique(chrchecks$MustContain[chrchecks$MustContain!="any character"])
  }else{
    MustContain <- "any character"
  }
  if(length(unique(chrchecks$DoesNotContain))>1){
    DoesNotContain <- paste(unique(chrchecks$DoesNotContain[chrchecks$DoesNotContain!=""]), collapse = "OR")
  }else if(length(unique(chrchecks$DoesNotContain[chrchecks$DoesNotContain!=""]))==1){
    DoesNotContain <- unique(chrchecks$DoesNotContain[chrchecks$DoesNotContain!=""])
  }else{
    DoesNotContain <- ""
  }

  chrchecks <- data.frame(DoesNotContain,MustContain)
  if (Sys.info()[['sysname']]!="Windows"){
    Pattern <- unlist(mclapply(c("before","after"), function(x) PatternFinder(SampleHeadwords,x,Data)))
  }else{
    Pattern <- unlist(lapply(c("before","after"), function(x) PatternFinder(SampleHeadwords,x,Data)))
  }
  Pattern <-data.frame(IsPrecededBy=Pattern[1],IsFollowedBy=Pattern[2])

  # CHECK TEMPLATE'S COLNAMES
  Template <- cbind(Pattern,chrchecks)
  Template$HeadExtractionMethod <- "pattern"
  Template$HeadValName <- ElementName
  Template <- Template[,c(6,5,1:4)]
  Template$ExtractStartingFrom <- ""
  Template$ExtractUpTo <- ""
  Template[is.na(Template)] <- ""
  return(Template)
}
AutoEntryExtractor <- function(EntriesBreakdown, DocSplit, DictTitle){

  for(col in colnames(EntriesBreakdown)){
    EntriesBreakdown[[col]] <- gsub("(^\\s+|\\s$)", "", EntriesBreakdown[[col]])
  }

  ExtractionRulesDF <- EntryElementExtractor(EntriesBreakdown[,1],DocSplit, colnames(EntriesBreakdown)[1])
  HeadValsDF <- ExctractEntryElementDF(EntryElementName=ExtractionRulesDF$HeadValName[1], DocScope=DocSplit,MustBeItemNumber=NULL, IsPrecededBy=ExtractionRulesDF$IsPrecededBy[1], IsFollowedBy=ExtractionRulesDF$IsFollowedBy[1],DoesNotContain=ExtractionRulesDF$DoesNotContain[1],MustContain=ExtractionRulesDF$MustContain[1])

  if(is.null(HeadValsDF)){
    # try to identify sencond element instead
    ExtractionRulesDF <- EntryElementExtractor(EntriesBreakdown[,2],DocSplit, colnames(EntriesBreakdown)[2])
    SecondValsDF <- ExctractEntryElementDF(EntryElementName=ExtractionRulesDF$HeadValName[1], DocScope=DocSplit,MustBeItemNumber=NULL, IsPrecededBy=ExtractionRulesDF$IsPrecededBy[1], IsFollowedBy=ExtractionRulesDF$IsFollowedBy[1],DoesNotContain=ExtractionRulesDF$DoesNotContain[1],MustContain=ExtractionRulesDF$MustContain[1], deduplicate=F)
    HeadDocSplit <- DocSplit[as.numeric(SecondValsDF$DocSplitIndex)-1]
    ExtractionRulesDF <- EntryElementExtractor(EntriesBreakdown[,1],HeadDocSplit, colnames(EntriesBreakdown)[1])
    HeadValsDF <- ExctractEntryElementDF(EntryElementName=ExtractionRulesDF$HeadValName[1], DocScope=DocSplit,MustBeItemNumber=NULL, IsPrecededBy=ExtractionRulesDF$IsPrecededBy[1], IsFollowedBy=ExtractionRulesDF$IsFollowedBy[1],DoesNotContain=ExtractionRulesDF$DoesNotContain[1],MustContain=ExtractionRulesDF$MustContain[1])
  }

  if(is.null(HeadValsDF)){
    print("cannot automatically identify entry extraction rules")
  }else{

    if(length(unique(HeadValsDF$DocSplitIndex))<nrow(HeadValsDF)){

      # if the HeadVal pattern occurs more then one time per paragraph the paragraph will be split

      DuplIndex <- HeadValsDF$DocSplitIndex[duplicated(HeadValsDF$DocSplitIndex)]
      # print(paste("line 172 DataConversionInterface: DocSplit[DuplIndex]=", DocSplit[DuplIndex]))
      # print(paste("line 173 DataConversionInterface: HeadValsDF$IsPrecededBy=", HeadValsDF$IsPrecededBy))

      Split <- unlist(lapply(DocSplit[DuplIndex], function(x) if(!is.null(x) && !is.null(HeadValsDF$IsPrecededBy) ){extracted <- unlist(str_split(x, HeadValsDF$IsPrecededBy))
      extracted[extracted!="<p>"]} ) )

      #print("line 178 DataConversionInterface")

      Split <- paste0(HeadValsDF$IsPrecededBy, Split)
      DocSplit <- DocSplit[-DuplIndex]
      DocSplit <- c(DocSplit, Split)
      HeadValsDF <- ExctractEntryElementDF(EntryElementName=ExtractionRulesDF()$HeadValName[1], DocScope=DocSplit,MustBeItemNumber=NULL, IsPrecededBy=ExtractionRulesDF()$IsPrecededBy[1], IsFollowedBy=ExtractionRulesDF()$IsFollowedBy[1],DoesNotContain=ExtractionRulesDF()$DoesNotContain[1],MustContain=ExtractionRulesDF()$MustContain[1])

    }

    # split DocSplit into individual entries
    EntriesList <- lapply(seq_along(HeadValsDF$DocSplitIndex[1:nrow(HeadValsDF)]), function(i) if(i < nrow(HeadValsDF)){DocSplit[HeadValsDF$DocSplitIndex[i]:(as.numeric(HeadValsDF$DocSplitIndex[i+1])-1)]}else{DocSplit[HeadValsDF$DocSplitIndex[i]:length(DocSplit)]} )
    names(EntriesList) <- HeadValsDF$Val
    saveRDS(EntriesList, "./WholeEntries.rds")
    write(jsonlite::toJSON(EntriesList, pretty = T), "./WholeEntries.json")

    OtherRules <- lapply(colnames(EntriesBreakdown)[2:length(EntriesBreakdown)], function(x) {print(x);EntryElementExtractor(EntriesBreakdown[[x]],DocSplit, x)})
    OtherRulesDF <- do.call(rbind, OtherRules)
    PositionalElements <- OtherRulesDF$HeadValName[OtherRulesDF$IsPrecededBy=="" & OtherRulesDF$IsFollowedBy==""]
    for(el in PositionalElements){
      OtherRulesDF$ExtractStartingFrom[OtherRulesDF$HeadValName==el] <- colnames(EntriesBreakdown)[which(colnames(EntriesBreakdown)==el)-1]
      OtherRulesDF$ExtractUpTo[OtherRulesDF$HeadValName==el] <- colnames(EntriesBreakdown)[which(colnames(EntriesBreakdown)==el)+1]
      OtherRulesDF$HeadExtractionMethod[OtherRulesDF$HeadValName==el] <- "position"
      OtherRulesDF$MustContain[OtherRulesDF$HeadValName==el] <-""
      OtherRulesDF$DoesNotContain[OtherRulesDF$HeadValName==el] <-""
    }
    OtherRulesDF$ExtractStartingFrom[is.na(OtherRulesDF$ExtractStartingFrom)] <- "beginning"
    OtherRulesDF$ExtractUpTo[is.na(OtherRulesDF$ExtractUpTo)] <- "end"
    ExtractionRulesDF <- rbind(ExtractionRulesDF, OtherRulesDF)
    if(length(which(ExtractionRulesDF$HeadExtractionMethod=="position"))>0){
      ExtractionRulesDF <- ExtractionRulesDF[c(which(ExtractionRulesDF$HeadExtractionMethod=="pattern"), which(ExtractionRulesDF$HeadExtractionMethod=="position")) ,]
    }
    write.csv(ExtractionRulesDF,"./EntryElementsExtractionRules.csv",row.names = F)

    EntryListByHead <-  HTMLentriesToListAndJson(ElementExtractionRules= ExtractionRulesDF , EntriesList= EntriesList, DictTitle= DictTitle)
    #print(" dataconversioninterface line 198")
    EntryListByHead <- do.call(rbind, EntryListByHead)
    #write("done","./done.txt")
  }
}


