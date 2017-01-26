# This R script is written to read in the plain contract texts

rm(list = ls())
libs <- c('tm','stringr')
lapply(libs,require,character.only = TRUE)

# Define the path and Term Document Matrix
path <- "G:/Codes/Text_Mining/Additional test documents all"
cont <- Corpus(DirSource(path,encoding = 'UTF-8',recursive = TRUE),readerControl = list(language='en'))

cont.cl <- tm_map(cont,content_transformer(tolower))

txt_vector <- vector()
stem.txt_list <- list()
for (i in 1:length(cont.cl)){
  temp.txt <- cont.cl[[i]]$content
  temp2.txt <- paste(temp.txt,collapse = ' ') # Connect the 
  temp3.txt <- gsub('\\t','',temp2.txt) # Delete the tab sign in texts
  txt_vector[i] <- stripWhitespace(temp3.txt)
}