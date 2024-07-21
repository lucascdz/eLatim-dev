LoadDictDataUI <- function(id, Label){
  tagList(
    fluidRow(
      column(width=6,
    textInput(NS(id,"dictDataPath"),
              label= Label)
      ),
    column(width=6,
           includeMarkdown("QuickGuide.Rmd")
           )
          )
  )

}



LoadDictDataServer <- function(id){
  moduleServer(id, function(input, output, session){

#print("LoadDictDataServer")

    Ext <- reactive({
      if(!is.null(input$dictDataPath)){
        gsub('^.*?\\.(zip|csv|tsv|rds|doc|docx|rtf|xml|txt|md|pdf)"?$', "\\1" , input$dictDataPath)
      }else{
        NULL
      }
    })


    Data <- reactive({
      if(!is.null(input$dictDataPath) && !is.null(Ext())){
        if(Ext() %in% c("zip","csv","tsv","rds","doc","docx","rtf" ,"xml","txt","md","pdf")){
        ReadDictDataWithZip(input$dictDataPath, Ext())
        }else{
          NULL
        }
      }else{
        NULL
      }
    })


    list(
      Contents = Data(),
      Ext = Ext()
    )
  })
}





