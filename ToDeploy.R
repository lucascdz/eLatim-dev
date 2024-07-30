
# # to deploy this app to shinyapps.io you need to raise the number of files allowed by rsconnect
# # uncomment the following to do so:
# 
# 
# options(rsconnect.max.bundle.files=length(list.files("./", full.names = TRUE, recursive = TRUE))+10, renv.config.dependencies.limit = length(list.files("./", full.names = TRUE, recursive = TRUE)))
#
# # then edit the bits in caps and run the deployment command below:
# 
# rsconnect::deployApp(appName="SHORT_DICT_NAME_WITHOUT_SPACES",appTitle="YOUR_DICT_NAME" , account="YOUR_SHINYAPPS_ACCOUNT_NAME")
# 
# # example DO NOT RUN:
# # rsconnect::deployApp(appName="DiachronicTibetanVerbValencyDictionary",appTitle="Diachronic dictionary of Tibetan verb valency" , account="mangalamresearch")


