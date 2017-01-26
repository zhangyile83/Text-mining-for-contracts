libs <- c('tm') # Just characters
lapply(libs,require,character.only = TRUE) # 'require' is a function that loads the packages named as 'libs'

Text = c("apple", "apples", "appleing")

stemDocument(Text)