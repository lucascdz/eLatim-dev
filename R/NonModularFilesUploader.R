NonModularFilesUI <- function(id){

  tagList(
    fileInput(NS(id,"CoverImg"), "upload png cover image (optional)", accept="png" ),
    fileInput(NS(id,"home"), "upload text for home page (only for shiny)", accept = c("txt","md","Rmd","docx")  ),
    fileInput(NS(id,"preface"), "upload preface (optional)", accept = c("txt","md","Rmd","docx")  ),
    fileInput(NS(id,"references"), "upload references (optional)", accept = c("txt","md","Rmd","docx")  )

  )

}


NonModularFilesServerQuarto <- function(id, DictName){

  moduleServer(id, function(input, output, session) {

    observeEvent(input$CoverImg,{
    ImgPath <-  input$CoverImg$datapath
    print(ImgPath)
    if(file.exists(paste0("./",DictName))){
    system(paste0("cp ",ImgPath, " ./",DictName,"/cover.png"))
    }
    })


    observeEvent(input$preface,{
      if(file.exists(paste0("./",DictName))){
      TextDataToQuarto(FilePath= input$preface$datapath, TargetPath=paste0("./",DictName,"/index.qmd") )
      }
    })

    observeEvent(input$references,{
      if(file.exists(paste0("./",DictName,))){
      TextDataToQuarto(FilePath= input$references$datapath, TargetPath=paste0("./",DictName,"/references.qmd") )
      }
    })

  })

}


NonModularFilesServerShiny <- function(id, DictName){

  moduleServer(id, function(input, output, session) {

    observeEvent(input$CoverImg,{
      ImgPath <-  input$CoverImg$datapath
      print(ImgPath)
      if(file.exists(DictName)){
        system(paste0('cwebp -q 80 ',ImgPath, ' -o ', './',DictName,'/cover.webp'))
      }
    })

    observeEvent(input$home,{
      if(file.exists(paste0("./",DictName))){
        TextDataToQuarto(FilePath= input$preface$datapath, TargetPath=paste0("./",DictName,"/Home.Rmd") )
      }
    })

    observeEvent(input$preface,{
      if(file.exists(paste0("./",DictName))){
        TextDataToQuarto(FilePath= input$preface$datapath, TargetPath=paste0("./",DictName,"/Preface.Rmd") )
      }
    })

    observeEvent(input$references,{
      if(file.exists(paste0("./",DictName,))){
        TextDataToQuarto(FilePath= input$references$datapath, TargetPath=paste0("./",DictName,"/References.Rmd") )
      }
    })

  })

}
