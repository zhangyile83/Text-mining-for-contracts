rm(list=ls())
libs <- c('tm','wordcloud','Rgraphviz')
lapply(libs,require, character.only = TRUE)

options(stringsAsFactors = FALSE)

# Define the path and Term Document Matrix
path <- "G:/Codes/Text_Mining/Additional test documents all"
cont <- Corpus(DirSource(path),readerControl = list(language='en'))

myStpwdlist <- c('shall','will','due','without','john','jane','doe')

#clean the corpus
cleanCorpus <- function(corpus){
  corpus_cl <- tm_map(corpus,content_transformer(gsub),pattern = ',',replacement = ' ')
  corpus_cl <- tm_map(corpus_cl,removePunctuation)
  corpus_cl <- tm_map(corpus_cl,stripWhitespace)
  corpus_cl <- tm_map(corpus_cl,content_transformer(tolower))
  corpus_cl <- tm_map(corpus_cl,removeWords,c(myStpwdlist,stopwords('english')))
  corpus_cl <- tm_map(corpus_cl,removeNumbers)
  
  result <- corpus_cl
}

# Stem the words in the corpus 
cont.cl <- cleanCorpus(cont)

cont.cl.st <- tm_map(cont.cl,stemDocument)
cont_TDM <- TermDocumentMatrix(cont.cl.st)
midname <- rownames(cont_TDM)

write.csv(cont_TDM, file = "TermDocumentMatrix.csv")

# cont_TDM <- removeSparseTerms(cont_TDM,0.7)
#remove the sparse terms in the TDM (higher number means lower threshold)

cleanCharacter <- function(char){
  char.cl <- tolower(char) # Transform the capital letters to lower forms
  char.cl <- union(char.cl,char.cl) # Take the same words as one

  result <- char.cl
}

# Create the dictionary for stemmed words completion
# Words are from http://www.talkenglish.com/vocabulary
stemdic <- read.csv('eng_dictionary.csv')
stemdic_word <- stemdic[,1]
stemdic_word <- as.character(stemdic_word)
stemdic_word.cl <- cleanCharacter(stemdic_word)

# Complete the stemmed words
thirdname <- stemCompletion(midname,dictionary = cont.cl,type = 'prevalent')
thirdname[thirdname==''] <- midname[thirdname=='']
fourthname <- stemDocument(thirdname)
compname <- stemCompletion(fourthname,dictionary = stemdic_word.cl,type = 'prevalent')
rownames(cont_TDM)[compname==''] <- thirdname[compname=='']
rownames(cont_TDM)[!compname=='']<- compname[!compname=='']
finalname <- rownames(cont_TDM)

contmtx <- as.matrix(cont_TDM)

# Choose the most frequent words in each document and write in a file
sink('Contract frequent word_stem_and_completed.txt')
for (i in 1:ncol(contmtx)){
  temp <- contmtx[,i]
  freqtemp <- names(which(temp>=3))
  cat(i,freqtemp,'\n')
}
sink()

# Draw the wordcloud
rsum <- rowSums(contmtx)
wordcloud(names(rsum[rsum>=10]),rsum[rsum>=10],colors = c('green','blue','yellow','red'),random.order = TRUE)

# Draw the Association graph between frequent words
freq.terms <- findFreqTerms(cont_TDM,lowfreq = 15)
plot(cont_TDM,term = freq.terms,corThreshold = 0.5,weighting = T)
#The freq.terms here should not be too much, or the plotting will fail

# Write the matrix into a csv file
## write.csv(t(contmtx),'Contract frequent matrix_stemmed_and_completed.csv')

# Find the words that only appear in one document
sink('Words in only one document.txt')
for (i in 1:nrow(contmtx)){
    tpvec <- contmtx[i,]
    if (length(tpvec[tpvec==0]) >= 10){
    cat(i,rownames(contmtx)[i],'\n')
   }
  }
sink()
#
# # Find the words that appear few in all documents
sparseWords <- finalname[rsum <= 2]
