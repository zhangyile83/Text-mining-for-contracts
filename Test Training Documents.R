#Init

rm(list=ls())
libs <-c('tm','plyr','class')
lapply(libs,require,character.only = TRUE) # If write 'library',only the first character
                                           # 'tm' will be inputted  

#Set options
options(stringsAsFactors = FALSE)

#Set path
ctrtype <-c('House Rental','Car') # Two types to be classified in
pathname <-'D:/Desktop/R work/Test Document' # Name of the path containing two type folders

#Corpus cleaning
cleanCorpus <- function(corpus){
  corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus,stripWhitespace)
  corpus <- tm_map(corpus,content_transformer(tolower)) # If we only write 'tolower' here, the format will
                                                        # be changed to characters rather than corpus
  corpus <- tm_map(corpus,removeWords,stopwords('english'))
  corpus <- tm_map(corpus,removeNumbers)
} 

#Generate TDM of the speeches
generate_TDM <- function(type,path){
  s.dir <- sprintf('%s/%s',path,type) 
  # sprintf is a function that outputs two strings of paths of each folder
  
  s.cor <- Corpus(DirSource(s.dir),readerControl = list(language = 'en')) 
  # Read in all documents in both folders to the Corpus
  
  s.cor <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor)
  
  s.tdm <- removeSparseTerms(s.tdm,0.7) # Remove the words with low frequency (but not know how to measure it)
  result <- list(tdm = s.tdm,name = type) # Output a list of tdm and name
}

tdm <- lapply(ctrtype,generate_TDM,pathname) # lapply is a function that outputs a vector
                                             # with the size of the first variable (ctrtype)

#Attach name to each row of TDM
bindContract_TDM <- function(tdm){
  s.mat <- t(data.matrix(tdm[['tdm']]))
  s.df <- as.data.frame(s.mat, stringsAsFactors = FALSE)
  
  s.df <- cbind(s.df,rep(tdm[['name']],nrow(s.df)))
  colnames(s.df)[ncol(s.df)]<-'targetcontract'
  # Let the final column be the type name of each document
  
  result <- s.df
  }

ctrtTDM <- lapply(tdm,bindContract_TDM)

#Stack
tdm.stack <- do.call(rbind.fill,ctrtTDM) # do.call is a function that runs a function on 
                                         # multiple variables (but only one here)
tdm.stack[is.na(tdm.stack)] <- 0 # Let all the NA in tdm.stack be 0

#Setting train and test samples
train.idx <- sample(nrow(tdm.stack),ceiling(nrow(tdm.stack)*0.5)) # Take 50% of all documents to be trainers
test.idx <- (1:nrow(tdm.stack))[-train.idx] # All the others are testers

#K-nearest-neighbour (knn) Model
tdm.ctrt <- tdm.stack[,'targetcontract']
tdm.stack.nl <- tdm.stack[,!colnames(tdm.stack) %in% 'targetcontract'] # Leave out the 'targetcontract' column

knn.pred <- knn(tdm.stack.nl[train.idx,],tdm.stack.nl[test.idx,],tdm.ctrt[train.idx],k=ceiling(sqrt(nrow(tdm.stack))))
# Run the Knn Algorithm with trainers and testers, with the known types of the trainers

#Accuracy measurement
conf.mat <- table('prediction'= knn.pred, Actual = tdm.ctrt[test.idx])
# Create a matrix of comparison between prediction and actual results

scsrate <- sum(diag(conf.mat))/length(test.idx)
# Measuring the success rate of the test

conf.mat;scsrate
