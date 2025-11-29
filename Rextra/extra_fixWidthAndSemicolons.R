qmdfolder <- './MyDict/'
filenames <- dir(qmdfolder,'\\.qmd')
filepaths <- paste0(qmdfolder,filenames)

FixQmdOutput <- function(qmdfilepath){
   #qmdfilepath <- './MyDict/capio.qmd'
   qmdfile <- readLines(qmdfilepath)

   qmdfile <- gsub('^; ','',qmdfile) %>%
      gsub('<br />;','<br>',.) %>%
      gsub('(wordcloud__.*width)=100% ','\\1=50% ',.)

   write(qmdfile,qmdfilepath)

}

lapply(seq_along(filepaths), function(i) FixQmdOutput(filepaths[i]))

# OK
