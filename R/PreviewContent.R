
# FIX ovrflowing text for doc files. 
#  overflow-x: scroll in tags$style does not seem to work

PreviewContentUI <- function(id){
  
 tagList(
  textOutput(NS(id, "TextPreview") ),
  htmlOutput(NS(id, "TextPreview2") ),
  tableOutput( NS(id, "Preview") )
)

}



PreviewContentServer <- function(id, DataContents){
  moduleServer(id, function(input, output, session){

     if(is.reactive(DataContents)){
       DataContents <- DataContents()
     }

if(!is.null(DataContents)){    
  

if(is.null(colnames(DataContents)) && str_detect(DataContents, "<p>")){
 
   if(length(DataContents)<2){
  
  DataContents <-  unlist(str_split(gsub("<p>","@@@@@<p>",DataContents), "@@@@@"))
 print("length(DataContents) from Preview") 
 print(length(DataContents)) 
 
  output$TextPreview <- renderText({
    if(length(DataContents)>50){
    print(DataContents[1:50])
    }else{
      print(DataContents)
    }
  })
  
  output$TextPreview2 <- renderText({
    if(length(DataContents)>50){
      print(DataContents[1:50])
    }else{
      print(DataContents)
    }
  })
  
  
   }else{
     output$TextPreview <- renderText({ 
       print(DataContents[1][[1]])
     })
     output$TextPreview2 <- renderText({ 
     print(DataContents[1][[1]])
     })
}

 
  }else{

  output$Preview <- renderTable({
    head(DataContents)

  })
  }
}
    })

}