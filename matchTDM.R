# This R script is used to match the words with a dictionary, trying to delete meaningless words

remainPOS = c("apple", "pear", "NOJIDJF")
words <- remainPOS

dic.pre <- read.csv('G:/Codes/Text_Mining/R script_Yile/eng_dictionary.csv')
dic <- dic.pre[,1]

dic.cl <- tolower(dic)
dic.cl <- unique(dic.cl)

words.temp_match <- base::match(words,dic.cl)
words.match.idx <- which(!is.na(words.temp_match))
words.match <- words[words.match.idx]
words.notmatch <- words[-words.match.idx]

print(words.match)
print(words.notmatch)
