


    BarchartEssentialUI <- function(id) {
      tagList(
        selectInput(NS(id, "barchartType"), "type of barchart (only for categorical variables)",
                    choices = c("fill","stack","dodge")),
        plotlyOutput( NS(id, "barchart")),
        radioButtons(
          NS(id, "VarsAsShades"),
          "view variables as (only for categorical variables)",
          choices= c('shades', 'colors'),
          selected='shades',
          inline = TRUE
        ),
        checkboxInput(
          NS(id, "doAll"),
          "plot all: SLOW, use with caution!", value=F)
      )
      #   actionButton(
      #     NS(id, "doAll"),
      #     "plot all: SLOW, use with caution!")
      # )
    }


    BarchartEssentialServer <- function(id,HeadwordVec, HeadwordVar, Dataset, FullData, xInput, fillInput, facetInput=NULL, xlabel="",ylabel="",LEMMA, Cores) {

      moduleServer(id, function(input, output, session) {


        observeEvent(c(input$VarsAsShades,input$barchartType) ,{
        # subcorpora freq chart:

          mainPlot <- plotBarcharts(Dataset=Dataset, xInput, fillInput, input$barchartType,input$VarsAsShades, facetInput=NULL,xlabel=as.character(xInput),ylabel="",LEMMA)

        output$barchart <- renderPlotly({
          ggplotly(mainPlot)

        })

        observeEvent(input$doAll,{
          if(input$doAll==T){

          if(!file.exists("./data/Plots")){
            system("mkdir ./data/Plots")
          }
            if(!file.exists("./data/AltText")){
              system("mkdir ./data/AltText")
            }

         # AllPlots <-  mclapply(HeadwordVec, function(x)
         #  plotBarcharts(Dataset=FullData[FullData[[HeadwordVar]]==x,-1], xInput, fillInput, input$barchartType,input$VarsAsShades, facetInput=NULL,xlabel="",ylabel=""), mc.cores = Cores
         #    )

            if(!HeadwordVar %in% colnames(FullData)){
              HeadwordVar <- "lemma"
              if("lemma" %in% colnames(FullData)){
                stop("error: FullData has not variable corresponding to either HeadwordVar or 'lemma' ")
              }
            }

         AllPlots <-  lapply(HeadwordVec, function(x)
          {print(x); plotBarcharts(Dataset=FullData[FullData[[HeadwordVar]]==x,], xInput, fillInput, input$barchartType,input$VarsAsShades, facetInput=NULL,xlabel=as.character(xInput),ylabel="",LEMMA=x)}
         )

         names(AllPlots) <- HeadwordVec
         #mclapply(seq_along(AllPlots), function(i) ggsave(paste0("./data/Plots/barchart_",MakeSafeForFilename(names(AllPlots)[i]),"_",as.character(xInput),"_BY_",as.character(fillInput),".png"),AllPlots[i][[1]],device = "png"), mc.cores = Cores)
         lapply(seq_along(AllPlots), function(i) {print(names(AllPlots)[i] );ggsave(paste0("./data/Plots/barchart_",MakeSafeForFilename(names(AllPlots)[i]),"_",as.character(xInput),"_BY_",as.character(fillInput),".png"),AllPlots[i][[1]],device = "png")})

         showModal(
           modalDialog(
             title = "done",
             "done! the wordclouds are in data > Plots",
             easyClose = TRUE,
             footer = NULL
           )
         )

         updateCheckboxInput(session,"doAll", value=F)
        }
        })

        })

        })

    }



