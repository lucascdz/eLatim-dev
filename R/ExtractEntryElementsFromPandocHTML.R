
# functions for extracting entry elements from pandoc-html (from Word conversion)

# need to test atterns with parenthesis and other escapable chars
Escape <- function(String){
  escaped <- str_replace_all(String,"(\\(|\\)|\\[|\\]|\\?|\\*|\\$|\\.|\\{|\\}|\\+)", paste0("@@@@@@" ,"\\1") )
  escaped <- gsub("@@@@@@","\\\\",escaped)
  escaped
}

EscapePunctuation <- function(String){
  #escapes punctuation AND "\\d" : for use in  in ExctractEntryElementDF() on regex pattern created with CreateRegex()

  escaped <- str_replace_all(String,"(\\!|\\?|\\.|\\\\d)", paste0("@@@@@@" ,"\\1") )
  escaped <- gsub("@@@@@@","\\\\",escaped)
  escaped
}

CreateRegex <- function(PatternDescription){

  Regex <- gsub("(\\*|\\?|\\!|\\[|\\]|\\(|\\)|\\{|\\}|\\.|\\^|\\$|\\+)" , "@@\\1", PatternDescription)%>%
    gsub("@@","\\\\",.)

  Regex <- gsub("any character","." ,Regex)
  Regex <- gsub("start of line","^" ,Regex) # if you run this on vector split on \n
  Regex <- gsub("end of line","$" ,Regex) # if you run this on vector split on \n
  Regex <- gsub("punctuation",'[\\\\.,;:\\\\!\\\\?]' ,Regex)
  Regex <- gsub("parentheses","[\\\\(\\\\)]" ,Regex)
  Regex <- gsub("brackets","[\\\\[\\\\]]" ,Regex)
  Regex <- gsub("curly brackets","[\\\\{\\\\}]" ,Regex)
  Regex <- gsub("angle","[<>]" ,Regex)
  Regex <- gsub("parentheses or brackets","[\\\\(\\\\)\\\\[\\\\]\\\\{\\\\}<>]" ,Regex)
  Regex <- gsub("any sequence of characters",".*?" ,Regex)
  Regex <- gsub("non-alphabetic","[^A-z]" ,Regex)

  Regex <- gsub("lowercase","[[:lower:]]" ,Regex)
  Regex <- gsub("uppercase","[[:upper:]]" ,Regex)
  Regex <- gsub("number","\\\\d+" ,Regex)
  Regex <- gsub("\\s","(\\\\s+)\\?", Regex)
  Regex <- gsub("optionalSpace","\\\\s?", Regex)
  Regex <- gsub("optionalCharacter","\\?", Regex)

  if(!is.null(Regex) && length(Regex)>0 && str_detect(Regex, "OR")){
    Regex <- paste0("(",Regex,")")
  }

  Regex <- gsub("OR","\\|", Regex)
  return(Regex)
}


PatternDefiner <- function(IsPrecededBy,IsFollowedBy){


  PatternRegex <- paste0(IsPrecededBy,".*?",IsFollowedBy)

  return(PatternRegex)
}

ExctractEntryElementDF <- function(EntryElementName, DocScope, MustBeItemNumber=NULL ,IsPrecededBy,IsFollowedBy,DoesNotContain=NULL,MustContain='any character', deduplicate=T){

  if(MustContain==""){
    MustContain <- "any character"
  }

  MustContain <- CreateRegex(MustContain)
  IsPrecededBy <- CreateRegex(IsPrecededBy)
  IsFollowedBy <- CreateRegex(IsFollowedBy)

  PatternRegex <- PatternDefiner(IsPrecededBy,IsFollowedBy)

  ExtractedVals <- unlist(lapply(DocScope, function(x) unlist(str_extract_all(x, PatternRegex)) %>%
                                   str_remove_all(., paste0("^",IsPrecededBy,"|",IsFollowedBy,"$"))))


  # remove tags from ExtractedVals
  ExtractedVals <- gsub("<.*?>","",ExtractedVals)
  ExtractedVals <- gsub("(^\\s+|\\s+$)","",ExtractedVals)

  # print(ExtractedVals)
  # print("line 67")
  if(!is.null(ExtractedVals) & length(ExtractedVals)>0){

    if (!is.null(MustContain) && MustContain!='any character'){
      if(str_detect(MustContain,"AND")){
        MustContain <- EscapePunctuation(MustContain)
        detectMustContain <-  paste0("str_detect(ExtractedVals,'",unlist(str_split(MustContain,"AND")),"')")
        detectMustContain <- paste(detectMustContain, collapse = " & ")
        ExtractedVals <- ExtractedVals[eval(parse(text=detectMustContain))]

      }else{
        ExtractedVals <- ExtractedVals[str_detect(ExtractedVals, MustContain)]
      }
    }
    if (!is.null(DoesNotContain) && !is.na(DoesNotContain) && length(DoesNotContain)>0 && DoesNotContain!=''){
      DoesNotContain <- CreateRegex(DoesNotContain)
      ExtractedVals <- ExtractedVals[!str_detect(ExtractedVals, DoesNotContain)]
    }
  }
  # print("line 73")
  if(!is.null(ExtractedVals) && length(ExtractedVals)>0){

    ExtractedValsIndex <- lapply(ExtractedVals, function(y) if(length(which(str_detect(unlist(DocScope), paste0(IsPrecededBy,Escape(y),IsFollowedBy)))>0)){data.frame(Val=y, DocSplitIndex=which(str_detect(unlist(DocScope), paste0(IsPrecededBy,Escape(y),IsFollowedBy))) )} )

    ExtractedValsIndex <- ExtractedValsIndex[!sapply(ExtractedValsIndex, is.null)]
    if(length(ExtractedValsIndex)>0){
      #print("line 80")
      DF <- ExtractedValsIndex[1][[1]]
      if(length(ExtractedValsIndex)>1){

        for(i in seq_along(ExtractedValsIndex)){

          if(nrow(ExtractedValsIndex[i][[1]])==1){
            DF <- rbind(DF,ExtractedValsIndex[i][[1]])
          }else if(nrow(ExtractedValsIndex[i][[1]])>1){

            ToKeep <- which(ExtractedValsIndex[i][[1]]$DocSplitIndex > DF$DocSplitIndex[nrow(DF)])
            ToKeep <- ToKeep[is.numeric(ToKeep)]
            if(length(ToKeep)>1){

              ToKeep <- ToKeep[which.min(ExtractedValsIndex[i][[1]]$DocSplitIndex[ToKeep]-DF$DocSplitIndex[nrow(DF)])]
            }

            DF <- rbind(DF,ExtractedValsIndex[i][[1]][ToKeep,])
          }

        }
      }
      DF <- data.frame(EntryElement=EntryElementName, DF)
      DF <- DF[!is.na(DF$DocSplitIndex),]
      #print("line 103")
      if(!is.null(MustBeItemNumber) && nrow(DF)>1){
        if(is.numeric(MustBeItemNumber)){
          if( MustBeItemNumber %in%  DF$DocSplitIndex){
            DF <-   DF[DF$DocSplitIndex==MustBeItemNumber,]
          }else{
            print("no string matching your search patterns at the specified Item Number")
          }
        }else{
          print("Item Number must be a digit")
        }
      }


      if(deduplicate==T){
        DuplicatedVals <- which(duplicated(DF$Val))
        #print("line 117")
        if(!is.null(DuplicatedVals) && length(DuplicatedVals)>0){
          DF <- DF[-DuplicatedVals,]
        }
      }
      #print("line 121")
      #DF
      return(DF)
    }
  }



}


ExtractElementBetween <- function(EntryElementName, DocSplit,ExtractStartingFrom="beginning",ExtractUpTo="end"){
  # ExtractStartingFrom can be the name of an element already defined (e.g. headword) or "beginning" (the start of the entry)
  # ExtractUpTo can be the name of an element already defined (e.g. headword) or "end" (the end of the entry)
  #print(EntryElementName)
  DocSplit <- unlist(DocSplit)
  DocSplitLength <- length(DocSplit)
  if(length(DocSplit)>1){
    if(ExtractStartingFrom!="beginning" && ExtractStartingFrom!=""){
      if(ExtractStartingFrom < DocSplitLength){
        ExtractionStartIndex <- ExtractStartingFrom +1
      }else{
        ExtractionStartIndex <- DocSplitLength
      }
    }else{
      ExtractionStartIndex <- 1
    }

    if(ExtractUpTo!="end" && ExtractUpTo!=""){
      ExtractionEndIndex <- ExtractUpTo-1
    }else{
      ExtractionEndIndex <- DocSplitLength

    }

    if(ExtractionEndIndex >= ExtractionStartIndex){
      ExtratedElement <- DocSplit[ExtractionStartIndex:ExtractionEndIndex]
      ExtractedValsIndex <- ExtractionStartIndex:ExtractionEndIndex
    }else{
      ExtratedElement <- NA
      ExtractedValsIndex <- NA
    }
  }else if(length(DocSplit)==1){
    ExtratedElement <- DocSplit
    ExtractedValsIndex <- NA
  }

  return(data.frame(EntryElement=EntryElementName, Val=ExtratedElement, DocSplitIndex=ExtractedValsIndex ))
}


ExtractElementsFromEntry <- function(entry, ElementExtractionRulesDF){

  if("pattern" %in% ElementExtractionRulesDF$HeadExtractionMethod){
    Elements <- lapply(ElementExtractionRulesDF$HeadValName[ElementExtractionRulesDF$HeadExtractionMethod=="pattern"], function(x) ExctractEntryElementDF(x, entry,MustBeItemNumber=NULL, ElementExtractionRulesDF[ElementExtractionRulesDF$HeadValName==x, 3],ElementExtractionRulesDF[ElementExtractionRulesDF$HeadValName==x, 4],ElementExtractionRulesDF[ElementExtractionRulesDF$HeadValName==x, 5],ElementExtractionRulesDF[ElementExtractionRulesDF$HeadValName==x, 6]) )
    names(Elements) <- ElementExtractionRulesDF$HeadValName[ElementExtractionRulesDF$HeadExtractionMethod=="pattern"]
    ElementsVals <- lapply(seq_along(Elements), function(i)  Elements[i][[1]]$Val)
    names(ElementsVals) <- names(Elements)
  }else{
    print("specify at list one element to be extracted by pattern")
  }
  if("position" %in% ElementExtractionRulesDF$HeadExtractionMethod){
    PositionElements <- lapply(ElementExtractionRulesDF$HeadValName[ElementExtractionRulesDF$HeadExtractionMethod=="position"],
                               function(x)  ExtractElementBetween(x, entry,
                                                                  ifelse(!is.null(Elements[[ElementExtractionRulesDF[ElementExtractionRulesDF$HeadValName==x, 7]]]),
                                                                         ifelse(ElementExtractionRulesDF[ElementExtractionRulesDF$HeadValName==x, 7]!="beginning",
                                                                                Elements[[ElementExtractionRulesDF[ElementExtractionRulesDF$HeadValName==x, 7]]]$DocSplitIndex,
                                                                                "beginning"),""),
                                                                  ifelse(!is.null(Elements[[ElementExtractionRulesDF[ElementExtractionRulesDF$HeadValName==x, 8]]]),
                                                                         ifelse(ElementExtractionRulesDF[ElementExtractionRulesDF$HeadValName==x, 8]!="end",
                                                                                Elements[[ElementExtractionRulesDF[ElementExtractionRulesDF$HeadValName==x, 8]]]$DocSplitIndex,
                                                                                "end"),"")
                               ))

    PositionElements <- lapply(seq_along(PositionElements), function(i)  PositionElements[i][[1]]$Val)
    names(PositionElements) <- ElementExtractionRulesDF$HeadValName[ElementExtractionRulesDF$HeadExtractionMethod=="position"]
    ElementsVals <- c(ElementsVals, PositionElements)
  }

  return(ElementsVals)

}



HTMLentriesToListAndJson <- function(ElementExtractionRules, EntriesList, DictTitle="dictData"){


#  print("HTMLentriesToListAndJson line 222")

  # if(!is.null(DictTitle) && !is.na(DictTitle) && nchar(DictTitle)<1 ){
  # DictTitle <- "MyDict"
  # }else if(!is.null(DictTitle) && !is.na(DictTitle) && nchar(DictTitle)>1 ){
  #   DictTitle <- gsub( " ","", DictTitle)
  # }

 # print("HTMLentriesToListAndJson line 230")

  #ElementExtractionRulesDF <- do.call(rbind, ElementExtractionRules)
  ElementExtractionRulesDF <- ElementExtractionRules
  ElementExtractionRulesDF[is.na(ElementExtractionRulesDF)] <- ""


  # apply Extraction rues to all entries to create named list for rds and json conversion
 # print("HTMLentriesToListAndJson line 239")
  if (Sys.info()[['sysname']]!="Windows"){
  ElementsByLemma <- mclapply(EntriesList,  function(x)  {print(x);ExtractElementsFromEntry(x, ElementExtractionRulesDF)} )
  }else{
  ElementsByLemma <- lapply(EntriesList,  function(x)  {print(x);ExtractElementsFromEntry(x, ElementExtractionRulesDF)} )
  }
 # print("HTMLentriesToListAndJson line 240")
   ElementsByLemma <- lapply(seq_along(ElementsByLemma), function(i) {print(paste("correctPositional",i));correctPositionalElements(ElementsByLemma[i][[1]],ElementExtractionRulesDF)} )
 #  print("HTMLentriesToListAndJson line 242")
  names(ElementsByLemma) <- names(EntriesList)

  # remove leading and training spaces:
  names(ElementsByLemma) <- gsub("^\\s+|\\s+$", "",  names(ElementsByLemma))
  # order alphabetically, in case some paragrahs were splitted and then added at the end of the list
 # ElementsByLemma <- ElementsByLemma[order(names(ElementsByLemma))]

  # convert to JSON
  EntriesDataJson <- jsonlite::toJSON(ElementsByLemma, pretty = T)

  EntriesDataJson <- gsub("<.?.?p>|<.?.?blockquote>"," ", EntriesDataJson)
  # write to file RDS and JSON
  write(EntriesDataJson, paste0("./data/",DictTitle,".json"))
  saveRDS(ElementsByLemma, paste0("./data/",DictTitle,".rds"))

  return(ElementsByLemma)
}


# SplitDoc <- function(SplitOnInput, Doc){
#
#   if(!is.null(Doc) && Doc !=""){
#     if(!is.null(SplitOnInput) && str_detect(SplitOnInput, "[A-z]") ){
#       SplitOn <- paste0("(",SplitOnInput,"|</p>|<br />)")
#     }else{
#       SplitOn <- "(</p>|<br />)"
#     }
#
#
#     DocSplit <-  unlist(str_split(gsub(SplitOn,"@@@@@\\1",Doc), "@@@@@"))
#
#     DocSplit <- DocSplit[!str_detect(DocSplit, paste0("^\\s?",SplitOn,"\\s?$"))]
#     DocSplit <- DocSplit[DocSplit!=""]
#
#     return(DocSplit)
#   }
# }

SplitDoc <- function(SplitOnInput, Doc){

  if(!is.null(SplitOnInput) && str_detect(SplitOnInput, "[A-z]") ){
    SplitOn <- SplitOnInput
    #SplitOn <- str_split(SplitOnInput,",\\s?")
    #SplitOn <- paste(c("<p>", SplitOn), collapse = "|")
  }else{
    SplitOn <- "(</p>|<br />)"
  }

  # print("YOU ARE SPLITTING ON:")
  # print(paste0("(",SplitOn,")"))

  #DocSplit <- unlist(str_split(gsub(paste0("(",SplitOn,")"),"\\1@@@@@",Doc), "@@@@@"))
  DocSplit <- unlist(str_split(Doc, paste0("(?<=",SplitOn,")")))
  DocSplit <- DocSplit[!str_detect(DocSplit, "^\\s?(<p>|</p>|<br />)\\s?$")]
  DocSplit <- DocSplit[DocSplit!=""]

  return(DocSplit)

}


correctPositionalElements <- function(StructuredEntry,ElementExtractionRulesDF){

  Elements <- ElementExtractionRulesDF$HeadValName[ElementExtractionRulesDF$HeadExtractionMethod=="position"]

  for(Element in Elements){

    if(!is.atomic(StructuredEntry[Element])){
      StructuredEntry[Element] <- paste(StructuredEntry[Element][[1]], collapse ="\n\n")
    }

    StartingFrom <- ElementExtractionRulesDF$ExtractStartingFrom[ElementExtractionRulesDF$HeadValName==Element]
    Upto <- ElementExtractionRulesDF$ExtractUpTo[ElementExtractionRulesDF$HeadValName==Element]
    #ElementExtractionRulesDF[ElementExtractionRulesDF$HeadValName==StartingFrom,]


    ToRemoveAtBeginning <- paste0(StructuredEntry[[StartingFrom]],ElementExtractionRulesDF$IsFollowedBy[ElementExtractionRulesDF$HeadValName==StartingFrom] )
    ToRemoveAtEnd <- paste0(ElementExtractionRulesDF$IsPrecededBy[ElementExtractionRulesDF$HeadValName==Upto], StructuredEntry[[Upto]])
    if(!is.null(ToRemoveAtBeginning) && sum(nchar(ToRemoveAtBeginning))>0){

      ToRemoveAtBeginning <- Escape(ToRemoveAtBeginning)


      ToRemoveAtBeginning <- paste0("^.*?",paste(ToRemoveAtBeginning[length(ToRemoveAtBeginning)], collapse=" "))

      StructuredEntry[Element] <- str_remove(StructuredEntry[Element], ToRemoveAtBeginning)
    }
    if(!is.null(ToRemoveAtEnd) && sum(nchar(ToRemoveAtEnd))>0){
      ToRemoveAtEnd <- Escape(ToRemoveAtEnd)
      ToRemoveAtEnd <- paste0(paste(ToRemoveAtEnd[1],collapse=" "),".*?$")

      StructuredEntry[Element] <- str_remove(StructuredEntry[Element], ToRemoveAtEnd)
    }

  }
  return(StructuredEntry)
}






