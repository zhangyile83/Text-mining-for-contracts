# This function is written to convert the raw .txt files into the term document matrix
rm(list=ls())
libs <- c('tm','wordcloud','Rgraphviz')
lapply(libs,require, character.only = TRUE)

# options(stringsAsFactors = FALSE)

# Define the path and Term Document Matrix
path <- "G:/Codes/Text_Mining/Additional test documents all"
cont <- Corpus(DirSource(path,encoding = 'UTF-8',recursive = TRUE),readerControl = list(language='en'))

# User defined stopword list, where the words in the list will be removed
myStpwdlist <- c('')

# Define a customized function cleanCorpus(Corpus)
cleanCorpus <- function(corpus){
  corpus_cl <- tm_map(corpus,content_transformer(gsub),pattern = '[[:punct:]]',replacement = ' ')
  corpus_cl <- tm_map(corpus_cl,stripWhitespace)
  corpus_cl <- tm_map(corpus_cl,content_transformer(tolower))
  corpus_cl <- tm_map(corpus_cl,removeWords,c(myStpwdlist,stopwords('english')))
  corpus_cl <- tm_map(corpus_cl,removeNumbers)
#  corpus_cl <- tm_map(corpus_cl, stemDocument)
  
  result <- corpus_cl
}


# Stem the words in the corpus 
cont.cl <- cleanCorpus(cont)

dtm <- DocumentTermMatrix(cont.cl)



# Entropy calculate function to find the non-feature words
EntropyCalculate <- function(wordmatrix){
wordrowsum <- rowSums(wordmatrix)
pmatrix <- wordmatrix/wordrowsum
logpmatrix <- log2(pmatrix)
logpmatrix[is.infinite(logpmatrix)] <- 0 # let 0*log2(0) be 0
wordentropymatrix <- pmatrix*logpmatrix # The * here is the direct multiplication, not matrix multiplication
wordentropy <- -rowSums(wordentropymatrix)

result <- wordentropy
}




dim_dtm <- dim(dtm)

TDM = inspect(dtm[1:dim_dtm[1], 1:dim_dtm[2]])

TDM <- t(TDM)

WordsEntropy = EntropyCalculate(TDM)

write.csv(TDM, file = "G:\\Codes\\Text_Mining\\R script_Yile\\TDM77.csv")

write.csv(WordsEntropy, file = "G:\\Codes\\Text_Mining\\R script_Yile\\Entropy77.csv")




