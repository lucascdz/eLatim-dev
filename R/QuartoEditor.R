
QuartoEditorUI <- function(id){
tagList(
 tabsetPanel(
   tabPanel("SetUpDict",
  selectInput(NS(id,"projectType"), "select project type", choices=c("book","shinyApp") ),
  textInput(NS(id,"author"),"author(s)"),
  textInput(NS(id,"title"),"dictionary title"),
  checkboxInput(NS(id,"go"), "set up dictionary", F),

  NonModularFilesUI(NS(id,"UploadNonModularFiles")),
 actionButton(NS(id,"includeCover"),"include cover image")
   ),
 tabPanel("EntryTemplate",
          EntryEditorUI(NS(id,"EntryEditor"))
 )

 )
)


}


QuartoEditorServer <- function(id,Cores){
moduleServer(id, function(input, output, session) {

  DictName <- "MyDict"
  if(file.exists("./data/dictData.rds")){
    DictData <- readRDS("./data/dictData.rds")
  }else{
    DictData <- NULL
  }



  observe({

    if(file.exists("./MyDict/DictCredits.txt")){
      updateSelectInput(session, "projectType", choices=c("book","shinyApp"),selected = "shinyApp")
    }


if(input$go==T){
 if(!file.exists("./MyDict/")){


  if(input$projectType=="book"){
  print("creating MyDict")
  system(paste("quarto create project", as.character(input$projectType), DictName))
  print(" MyDict done")

    req(paste0("./", DictName))

    if (Sys.info()[['sysname']]!="Windows"){

      system("rm ./MyDict/summary.qmd ./MyDict/intro.qmd")
    }else{
      system('powershell.exe', input= "rm .\\MyDict\\summary.qmd", intern=T)
      system('powershell.exe', input= "rm .\\MyDict\\intro.qmd", intern=T)
    }

    yamlFile <- readtext::readtext("./MyDict/_quarto.yml")$text
    yamlFile <- gsub("\n(\\s+|\t+)theme: cosmo\n","\n\\1theme: cosmo\n\\1toc: true\n\\1toc-depth: 4\n", yamlFile)
    if(!is.null(input$author) && str_detect(input$author, "[A-z]")){
      yamlFile <- gsub('author: ".*?"\n',paste0('author: "',input$author,'"\n'),  yamlFile)
    }
    if(!is.null(input$title) && str_detect(input$title, "[A-z]")){
      yamlFile <- gsub('title: ".*?"\n',paste0('title: "',input$title,'"\n'),  yamlFile)
    }
    yamlFile <- gsub("- ([Ss]ummary|[Ii]ntro).qmd\n","", yamlFile)
    yamlFile <- gsub("\n\\s\\s+-", "\n  -", yamlFile)
    yamlFile <- gsub("  pdf:
    documentclass: scrreprt", "\n", yamlFile)

    write(yamlFile, "./MyDict/_quarto.yml")

 }else if(input$projectType=="shinyApp"){
  print("creating MyDict")
  system("mkdir ./MyDict")
  system("mkdir ./MyDict/data")
  if(!is.null(input$title) & !is.null(input$author)){
  write(paste0('title=',input$title,' author=' , input$author), "./MyDict/DictCredits.txt")
  }else{
    write(paste0('title=',"a dictionary",' author=' , "unknown"), "./MyDict/DictCredits.txt")
  }
  print(" MyDict done")

 }

}else{
  showModal(
    modalDialog(
      title = "MyDict exists already",
      "if you wish to create a new dictionary remove MyDict folder from the app directory",
      easyClose = TRUE,
      footer = NULL
    )
  )
}

      updateCheckboxInput(session, "go", value=F)
}

  if(input$projectType=="book" && file.exists("./MyDict/_quarto.yml")){
    #if(input$projectType=="book"){

  NonModularFilesServerQuarto("UploadNonModularFiles",DictName=DictName)

  observeEvent(input$includeCover,{
    if(file.exists("./MyDict/index.qmd") & file.exists("./MyDict/cover.png")){
    preface <- readtext::readtext("./MyDict/index.qmd")$text
    write(paste("![](cover.png)" ,preface, sep="\n\n"), "./MyDict/index.qmd")
  }
  })

  EntryEditorServer("EntryEditor", DictData, Cores)

  }else if(input$projectType=="shinyApp" && file.exists("./MyDict/DictCredits.txt")){
    NonModularFilesServerShiny("UploadNonModularFiles",DictName=DictName)

    observeEvent(input$includeCover,{
      if(file.exists("./MyDict/Home.Rmd") & file.exists("./MyDict/cover.webp")){
        Home <- readtext::readtext("./MyDict/Home.Rmd")$text
        write(paste("![](cover.png)", preface, sep="\n\n"), "./MyDict/Home.Rmd")
      }
    })

    EntryEditorServerSHINY("EntryEditor",DictData, Cores)

  }

  })
})
}
