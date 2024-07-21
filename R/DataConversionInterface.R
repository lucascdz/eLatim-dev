
# module to display relevant menus for csv or word conversion
# AND actually convert the file
# by clicking on the Button generated in this module
# users write a json and a rds version of their data.
# the files get written in the root directory of the app.


# ADD HANDLING OF WORD FILES

TabularDataConversionInterfaceUI <- function(id){


  tagList(
  #  textInput(NS(id,"DictName"), "dictionary name", placeholder = "keep it short!"),

    # radioButtons(NS(id,"MultiEntryFiles"),
    #              "Please select the following only once:\n I have",
    #              choices= c("multiple entries per file",
    #                         "One entry per file"),
    #              selected = "One entry per file"
    # ),

    uiOutput(NS(id,"uploadExtractionRules")),

    uiOutput(NS(id,"ConversionControls")),

    htmlOutput(NS(id,"SampleEntry")),

    uiOutput(NS(id,"ConvertButton"))
    #checkboxInput(NS(id,"ConvertButton"), value=F)
  )

}



TabularDataConversionInterfaceServer <- function(id, Ext, Data){
  moduleServer(id, function(input, output, session){



    observe({

      if(is.reactive(Ext)){
        Ext <- Ext()
      }

      if(is.reactive(Data)){
        Data <- Data()
      }


      if (!is.null(Ext) && length(Ext)>0){


        if(Ext %in% c("csv","tsv","rds")){


          output$ConversionControls <- renderUI({

            tagList(
              selectInput(NS(id,"HeadCol"),
                          "column with entries's heads (e.g. the headword)",
                          choices = colnames(Data)
              ),

              radioButtons(NS(id,"mergeMethod"),
                           label="to handle duplicated entry heads",
                           choices= c("merge",
                                      "addNumber"),
                           selected="merge"),

              textInput(NS(id,"AdditionalDataPath"),
                        "Path to additional tabular data"),

              textInput(NS(id,"ColToJoinBy"),
                        "column to join data by (must have the same name in all files):")

            )
          })


          observeEvent( c(input$HeadCol, input$mergeMethod,input$AdditionalDataPath, input$ColToJoinBy),{



            if(!is.null(input$HeadCol) && length(input$HeadCol)>0 ){




              output$ConvertButton <- renderUI({

                actionButton(NS(id,"Convert"),
                             "Convert to Rlist & JSON"#,
                )
              })

                }


          })


         observeEvent(input$Convert,{

          ConvertToRDSandJSON(DictData= Data, DictName= "dictData", DataHeadCol = input$HeadCol, HeadDuplicateHandling= input$mergeMethod, AdditionalDataPath= input$AdditionalDataPath, ColToJoinBy=input$ColToJoinBy)

        })

        }else{


           #if(input$MultiEntryFiles=="One entry per file"){




            output$uploadExtractionRules <- renderUI({
              tagList(
                fileInput( NS(id, "uploadEntryBreakdown"),
                           label=  "upload csv with entry breakdown (see user guide)",
                           accept = ".csv"),
                #downloadBttn(NS(id, "downloadExtractionRulesTemplate"), "OR download Extraction Rules template"),

                fileInput( NS(id, "uploadExtractionRulesDF"),
                           label=  "upload csv with rules to extract entry elements",
                           accept = ".csv"),
                #checkboxInput( NS(id,"ReadyToConvert"),"convert to RDS & JSON ", F)
                actionButton( NS(id,"ReadyToConvert"),"convert to RDS & JSON ")

              )

            })


            # output$downloadExtractionRulesTemplate <- downloadHandler(
            #   filename = function() {
            #     "EntryElementsExtractionRulesTemplate.csv"
            #   },
            #
            #   content = function(file) {
            #     Template <- data.frame( HeadValName="", HeadExtractionMethod="", IsPrecededBy="", IsFollowedBy="", DoesNotContain="", MustContain="", ExtractStartingFrom="", ExtractUpTo="")
            #     write.csv(Template, file, row.names = F)
            #   }
            # )



            EntryBreakdownDF <- reactive({
              if(!is.null(input$uploadEntryBreakdown)){
                read.csv(input$uploadEntryBreakdown$datapath, header=T, stringsAsFactors = F)
              }else{
                NULL
              }
            })

            if(!is.null(EntryBreakdownDF())){

              showModal(modalDialog(
                title = "processing",

                div(HTML("please wait")),

                easyClose = FALSE
              ))
              DocSplit <- SplitDoc("", Data)
              AutoEntryExtractor(EntryBreakdownDF(), DocSplit, "dictData")
              print("all processed")
              showModal(modalDialog(
                title = "done",

                div(HTML("check EntryElementsExtractionRules.csv & dictData.json in root directory")),

                easyClose = FALSE
              ))

            }else{
            EntryElementsExtractionRulesTemplate <- data.frame(HeadValName="", HeadExtractionMethod="", IsPrecededBy="", IsFollowedBy="",DoesNotContain="",MustContain="",ExtractStartingFrom="",ExtractUpTo="" )
                write.csv(EntryElementsExtractionRulesTemplate ,"./EntryElementsExtractionRulesTemplate.csv", row.names = F)
            }


            ExtractionRulesDF <- reactive({
              if(!is.null(input$uploadExtractionRulesDF)){
                read.csv(input$uploadExtractionRulesDF$datapath, header=T, stringsAsFactors = F)

              }else{
                NULL
              }
            })


            output$ConversionControls <- renderUI({

                tagList(
                textInput(NS(id,"SectionIdentifier"), "any section identifier other than end of paragraph (</p> or <br />). Separate values by '|' (e.g <p>|</blockquote>)."),
                              )

            })





                               observeEvent(input$ReadyToConvert,{

                                   DocSplit <- SplitDoc(input$SectionIdentifier, Data)


                                   #HeadValsDF <- ExctractEntryElementDF(EntryElementName=input$HeadValName, DocScope=DocSplit,MustBeItemNumber=NULL, IsPrecededBy=input$IsPrecededBy, IsFollowedBy=input$IsFollowedBy,DoesNotContain=input$DoesNotContain,MustContain=input$MustContain)

                                   HeadValsDF <- ExctractEntryElementDF(EntryElementName=ExtractionRulesDF()$HeadValName[1], DocScope=DocSplit,MustBeItemNumber=NULL, IsPrecededBy=ExtractionRulesDF()$IsPrecededBy[1], IsFollowedBy=ExtractionRulesDF()$IsFollowedBy[1],DoesNotContain=ExtractionRulesDF()$DoesNotContain[1],MustContain=ExtractionRulesDF()$MustContain[1])
                                   # print("HeadValsDF")
                                   # print(HeadValsDF)
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
                                   # display resulting entry to user
                                   saveRDS(EntriesList, "./WholeEntries.rds")
                                   write(jsonlite::toJSON(EntriesList, pretty = T), "./WholeEntries.json")

                                   #print(" dataconversioninterface line 195")

                                   EntryListByHead <-  HTMLentriesToListAndJson(ElementExtractionRules= ExtractionRulesDF() , EntriesList= EntriesList, DictTitle= "dictData")
                                   #print(" dataconversioninterface line 198")
                                   EntryListByHead <- do.call(rbind, EntryListByHead)

                              print("END")

                                 #}
                               })

                             #}

                          # })


          #}


        }
      }
    })
  })
}




