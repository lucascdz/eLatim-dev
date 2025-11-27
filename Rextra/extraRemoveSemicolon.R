library(htmltools)


test <- readLines('/Users/lucascdz/lucascdz@github.com/eLatim copy/debeo.html')

test <- gsub('> ; ','> ',test)

write(test,'~/Desktop/test.html')

