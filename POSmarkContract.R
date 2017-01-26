# This R script is written to recognize and delete the verbs and adjectives in the contract texts

libs <- c('tm','wordcloud','Rgraphviz','stringr','NLP','openNLP')
lapply(libs,require, character.only = TRUE)

options(stringsAsFactors = FALSE)

stem.word_list_etp <- stem.word_list
stem.txt_list_etp <- stem.txt_list
# Define the text vector according to the stemmed word texts
word_cat <- paste(stem.word_list_etp,collapse = ' ')

txt_vector <- vector()
for (i in 1:length(stem.txt_list_etp)){
  txt_vector[i] <- paste(stem.txt_list_etp[[i]],collapse = ' ') # Connect the words to be a character
}

# This function can mark every word with its POS, NN -> noun, JJ -> adjective, VRB -> verb, etc.
# This function comes from 'http://stackoverflow.com/questions/28764056/could-not-find-function-tagpos', no need to know the details
tagPOS <-  function(x, ...) { 
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}

# The commands below are used to find nouns in word list, and then delete words other than nouns in contracts
# Choose one between this method and the one in line 54
  #Filter the words that are nouns
POS_word <- tagPOS(word_cat)
POS_word.non_idx <- which(str_detect(POS_word$POStags,'^N'))
POS_word.adj_idx <- which(str_detect(POS_word$POStags,'^J'))
POS_word.vrb_idx <- which(str_detect(POS_word$POStags,'^V'))

POS_word.non <- stem.word_list_etp[POS_word.non_idx]
word_text_matrix <- word_text_matrix[POS_word.non_idx,]

  # Take the nouns above to match all nouns in contracts
stem.txt_list_etp_non <- list()
for (i in 1:length(stem.txt_list_etp)){
  temp.match_non <- lapply(paste('^',POS_word.non,'$',collapse = ' '),str_detect,stem.txt_list[[i]]) # detect the chosen words in contracts
  temp.match_non_idx <- unlist(lapply(temp.match_non,which)) # Locate the whereabouts of deleted words
  temp.match_non_idx <- union(temp.match_non_idx,temp.match_non_idx) # Delete repeated elements
  
  stem.txt_list_etp_non[[i]] <- stem.txt_list_etp[temp.match_non_idx]
}

# The commands below are used to find nouns in every contract, which is slower than the method above ()
POS_txt <- lapply(txt_vector,tagPOS)
POS_non_txt <- list()
POS_adj_txt <- list()
POS_vrb_txt <- list() # create empty lists
for (i in 1:length(POS_txt)){
  temp.non_idx <- which(str_detect(POS_txt[[i]]$POStags,'^N')) # The tags started by N (means NN(noun),NNP(plural),etc.)
  temp.adj_idx <- which(str_detect(POS_txt[[i]]$POStags,'^J')) # Similar to above
  temp.vrb_idx <- which(str_detect(POS_txt[[i]]$POStags,'^V')) # Similar to above
# list the three divisions to check if there are any mis-classified words
  POS_non_txt[[i]] <- stem.txt_list_etp[[i]][temp.non_idx] # This is what we want for further classification
  POS_adj_txt[[i]] <- stem.txt_list_etp[[i]][temp.adj_idx]
  POS_vrb_txt[[i]] <- stem.txt_list_etp[[i]][temp.vrb_idx]
}
stem.txt_list_etp_non <- POS_non_txt
