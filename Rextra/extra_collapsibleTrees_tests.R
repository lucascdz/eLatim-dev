TreeDF <- read.csv('/Users/lucascdz/Rprojects/iLexicographR/data/LexicalData_top999/lexidata_wordnetLexemes.csv')

TreeList <- split(TreeDF,TreeDF$lemma)

collapsibleTree(TreeList[['appello']],
                hierarchy = c('sem_type','definition_pt','lexemes_pt'),
                root = names(TreeList['appello']),
                #attribute = "n_lex",
                #nodeSize = "hits",
                fontSize= 15,
                #tooltip=T,
                collapsed = T,
                width = 800,
                height = 600,
                zoomable = T
)


### EXAMPLE IN COLORS

species <- read.csv("https://apps.fs.usda.gov/fia/datamart/CSV/REF_SPECIES_GROUP.csv")

collapsibleTree(
   species,
   hierarchy = c("REGION", "CLASS", "NAME"),
   fill = c(
      # The root
      "seashell",
      # Unique regions
      rep("brown", length(unique(species$REGION))),
      # Unique classes per region
      rep("khaki", length(unique(paste(species$REGION, species$CLASS)))),
      # Unique names per region
      rep("forestgreen", length(unique(paste(species$NAME, species$REGION))))
   )
)



### EXAMPLE
parent <- c(NA, "Carlos", "Carlos", "María", "María", "Paula", "Alex")
child <- c("Carlos", "María", "Alex", "Javier", "Paula", "Pablo", "Pepe")

df <- data.frame(parent, child)

library(collapsibleTree)
collapsibleTreeNetwork(df, collapsed = FALSE)
