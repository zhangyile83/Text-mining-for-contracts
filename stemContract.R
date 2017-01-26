# This R script is written to realize the stemming process of any contracts read in

rm(list = ls()) # Remove all variables in the Global Environment

# load the required packages 
libs <- c('tm','stringr') # Just characters
lapply(libs,require,character.only = TRUE) # 'require' is a function that loads the packages named as 'libs'

options(stringsAsFactors = FALSE)

# Construct the corpus based on the path
path <- "D:/Desktop/Additional test documents all"

stemContract <- function(path){
  cont <- Corpus(DirSource(path,encoding = 'UTF-8'),readerControl = list(language='en'))# Note the txt file is coded in UTF-8
  
  # If the path includes several folders, it should be written as
  # cont <- Corpus(DirSource(path,encoding = 'UTF-8',recursive = TRUE),readerControl = list(language='en'))
  
  #clean the corpus: remove punctuations,numbers,extra spaces, and turn capital letters to lower
  cleanCorpus <- function(corpus){
    corpus_cl <- tm_map(corpus,content_transformer(gsub),pattern = '[[:punct:]]',replacement = ' ')
    corpus_cl <- tm_map(corpus_cl,content_transformer(gsub),pattern = '[0-9]',replacement = ' ')
    # Using 'gsub' function instead of 'remove~' so that no words would be connected when punctuations and numbers are deleted
    corpus_cl <- tm_map(corpus_cl,stripWhitespace) # Delete extra space (' ')
    corpus_cl <- tm_map(corpus_cl,content_transformer(tolower)) # Turn all capital letters to lower
    
    result <- corpus_cl # Can also write 'return(corpus_cl)'
  }
  
  # Read in the contracts
  cont.cl <- cleanCorpus(cont) # clean the corpus 
  
  # Stem the words in the corpus 
  beforestem.txt <- list()
  stem.txt_list <- list() # create empty lists
  (time <- Sys.time()) # Record time
  for (i in 1:length(cont.cl)){
    # Read in the original text
    temp.txt <- cont.cl[[i]]$content # Read in text in character vector format
    temp2.txt <- paste(temp.txt,collapse = ' ') # Connect all elements in temp.txt
    temp3.txt <- stripWhitespace(temp2.txt) # temp3.txt saves the plain contract text in one character
    # Stem the text by deleting punctuation, spliting and then pasting
    stem.temp.txt <- str_split(temp3.txt,' ')[[1]][-length(stem.temp.txt)] # Cut every word as an element; the tail element of stem.temp.txt is empty, so delete it
    
    # Delete the words that has length 1 in every contract (This part can be deleted)
    #temp.word_leng <- lapply(stem.temp.txt,nchar) # Measure the lengths of every word
    #temp.dlt_idx <- which(temp.word_leng==1) # Record the id of word that has only one letter
    #beforestem.txt[[i]] <- stem.temp.txt[-temp.dlt_idx] # Delete the words
    
    beforestem.txt[[i]] <- stem.temp.txt # Record beforestem.txt for subsequent word completion
    stem.txt <- stemDocument(beforestem.txt[[i]]) # Conduct stemming to every word in stem.temp3.txt 
    stem.txt_list[[i]] <- as.character(stem.txt)
    print(Sys.time())# Record the system time in every round
  }
  #rm(temp.txt,temp2.txt,temp3.txt,stem.temp.txt,stem.temp2.txt,stem.txt,after.stem,after.stem2)
  # Remove all temp variables
  
  # Conduct the completion process once and for all, and substitute the stemmed words in every contract
  # Create the word list that has been stemmed
  temp.word_list <- unlist(stem.txt_list) # Save the stemmed words in every contracts in a list
  temp.word_list <- union(temp.word_list,temp.word_list) # Delete the repeated words
  stem.word_list <- temp.word_list
  
  # The following commands are used to complete the stemmed words
  # stem.word_list <- stemCompletion(temp.word_list,dictionary = cont.cl,type = 'prevalent')
  # stem2.word_list <- union(stem.word_list,stem.word_list)
  
  # # Substitute the stemmed words with completed words or words in original contracts
  # for (j in 1:length(temp.word_list)){ # For every stemmed word, substitute it with completed words
  #   for (k in 1:length(stem.txt_list)){ # in every contract
  #     if (stem.word_list[j]==''){ # If the stemmed word fails to be completed
  #       fail.temp.idx <- which(str_detect(stem.txt_list[[k]],temp.word_list[j])) # Mark the id of those words in the contract
  #       if (length(fail.temp.idx)){
  #         gsub(temp.word_list[j],beforestem.txt[[k]][fail.temp.idx],stem.txt_list[[k]]) # substitute the word with the original word, instead of the completion word (which is empty)
  #           }
  #       } else {
  #     gsub(temp.word_list[j],stem.word_list[j],stem.txt_list[[k]]) # Take the completed words to substitute original words
  #     }
  #   }
  # }
  return(stem.word_list)
}
