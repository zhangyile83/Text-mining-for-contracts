# This R script is used to test the POS recognition to Term Document Matrix
rm(list = ls())
libs <- c('tm','Rgraphviz','stringr','NLP','openNLP')
lapply(libs,require, character.only = TRUE)

options(stringsAsFactors = FALSE)

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
  return(POStags)
}

#beforePOS <- rownames(TDM)
A2 <- read.csv('G:/Codes/Text_Mining/R script_v3/A2_temp.csv')
beforePOS <- A2[,'X']
afterPOS <- tagPOS(paste(beforePOS,collapse = ' '))

POS_word.non_idx <- which(str_detect(afterPOS,'^N'))
POS_word.non <- beforePOS[POS_word.non_idx]
POS_word.vrb_idx <- which(str_detect(afterPOS,'^V'))
POS_word.vrb <- beforePOS[POS_word.vrb_idx]
remainPOS <- beforePOS[union(POS_word.non_idx,POS_word.vrb_idx)]
