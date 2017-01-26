# This R script is used to delete the non-sparse words in all contracts

libs <- c('tm','wordcloud','Rgraphviz','stringr')
lapply(libs,require, character.only = TRUE)

# Calculate the term document matrix by ourselves
word_text_matrix <- matrix(data = 0,nrow = length(stem.word_list),ncol = length(cont.cl)) # Define a zero matrix
for(i in 1:ncol(word_text_matrix)){ # Go through every column of the matrix (contract)
  for(j in 1:nrow(word_text_matrix)){ # Go through every row of the matrix (word)
    starttime <- Sys.time() # Record start time
    temp <-str_match_all(stem.txt_list[[i]], paste('^',stem.word_list[j],'$',sep = '')) # match the words
    # Save the word in contracts that appear in the word_list
    temp2 <- unlist(temp) # Length of this variable is the frequency that word appears
    word_text_matrix[j,i] <- length(temp2)
    temptime <- Sys.time();print(paste('[',j,']','[',i,']',temptime-starttime))# Record time
  }
}
write.csv(word_text_matrix,'TermDocumentMatrix.csv')
#rm(temp,temp2) # Remove the temp variables

# The frequency statistics can be done here, commands from frequencyContract.R
  # Draw the wordcloud
word_rowsum <- rowSums(word_text_matrix) # Frequency of every word
freq.threshold <- quantile(word_rowsum)[4] # Select the 25% most frequent words to be in wordcloud
freq.idx <- which(word_rowsum>=freq.threshold) # Find the id of the frequent words
freq.terms <- afterstem.word_list[freq.idx]
# The freq.terms here should not be too much, or the Association graph plotting will fail
wordcloud(stem.word_list[freq.idx],word_rowsum[freq.idx],colors = c('green','blue','yellow','red'),random.order = TRUE)

  # Draw the Association graph between frequent words
 # Find frequent words in TDM after deleting elements
#plot(cont_TDM,term = freq.terms,corThreshold = 0.5,weighting = T) # Problems remained to be solved

# Find the words that are even enough to be deleted
EntropyCalculate <- function(wordmatrix){
  wordrowsum <- rowSums(wordmatrix)
  pmatrix <- wordmatrix/wordrowsum
  logpmatrix <- log2(pmatrix)
  logpmatrix[is.infinite(logpmatrix)] <- 0 # let 0*log2(0) be 0
  wordentropymatrix <- pmatrix*logpmatrix # The * here is the direct multiplication, not matrix multiplication
  wordentropy <- -rowSums(wordentropymatrix)
  
  result <- wordentropy
}

stem.word_list.entropy <- EntropyCalculate(word_text_matrix)
names(stem.word_list.entropy) <- stem.word_list
write.csv(stem.word_list.entropy,'Entropy.csv')

# Choose those words with very high information entropy (so even that contain low information)
stem.word_list.entropy_dlt_idx <- which(stem.word_list.entropy>=quantile(stem.word_list.entropy)[4])
stem.word_list.delete <- stem.word_list[stem.word_list.entropy_dlt_idx]
stem.word_list_etp <- stem.word_list[-stem.word_list.entropy_dlt_idx]

stem.txt_list_etp <- list()
# Let the contracts delete the words chosen above
for (i in 1:length(stem.txt_list)){
    temp.match_dlt <- lapply(paste('^',stem.word_list.delete,'$',sep = ''),str_detect,stem.txt_list[[i]]) # detect the chosen words in contracts
    temp.match_dlt_idx <- unlist(lapply(temp.match_dlt,which)) # Locate the whereabouts of deleted words
    temp.match_dlt_idx <- union(temp.match_dlt_idx,temp.match_dlt_idx) # Delete repeated elements
    stem.txt_list_etp[[i]] <- stem.txt_list[[i]][-temp.match_dlt_idx] # Delete words in every contract
    word_text_matrix <- word_text_matrix[-temp.match_dlt_idx,]
}
#rm(temp.match_dlt,temp.match_dlt_idx)
