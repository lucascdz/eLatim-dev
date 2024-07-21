

stringrInst <-  require(stringr)
if(stringrInst!=T){
  install.packages('stringr',repos='http://cran.us.r-project.org',dependencies = T,type='source')
}
pkgloadInst <-  require(pkgload)
if(pkgloadInst!=T){
  install.packages('pkgload',repos='http://cran.us.r-project.org',dependencies = T, type='source')
}

if(stringr::str_detect(getwd(),"./MyDict$")){
  print(getwd())
  setwd(paste0("../"))
  getwd()
}

source("./R/SetUp.R")
pkgload::load_all(".")
source("MyApp.R")
myApp()



